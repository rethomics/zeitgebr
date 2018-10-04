rm(list=ls());gc()
options(nwarnings = 1000)
library(scopr)
library(ggetho)
library(sleepr)
library(gtools)
#library(TSclust)

FEMALE_MALE_PALETTE <- c("#be2828ff", "#282896ff")
CONTROL_SD_PALETTE <- c( "#969696ff", "#3caa3cff")

METADATA <- "metadata.csv"
CACHE <- "./cache/"
#~ RESULT_DIR <- "./raw_results/"
RESULT_DIR <- "/data/ethoscope_results"

REMOTE_DIR <- "ftp://nas.lab.gilest.ro/auto_generated_data/ethoscope_results/"

met <- fread(METADATA)
met <- met[status == "OK" & sdi == 0]

met <- link_ethoscope_metadata(met, result_dir = RESULT_DIR)


dt <- load_ethoscope(met,
                     max_time=days(7),
                     reference_hour=9.0,
                     cache = CACHE,
                     FUN = sleep_annotation,
                     ncores=1)


curate_data <- function(data){
  #data[, t := t - days(xmv(baseline_days))]
  #data <- data[is_interpolated == F]
  # first we remove animals that do not have enought data points
  valid_animals <- data[,.(t_range = max(t) - min(t)), by=id][t_range >= days(5)]$id
  data <- data[t > days(1) &
                 t < days(6) &
                 id %in% valid_animals]
  data[, t := t - days(1)]

  data[,x_rel:=ifelse(xmv(region_id) > 10, 1-x, x)]
  data[,x_rel:=ifelse(xmv(inverted), 1-x_rel, x_rel)]
  norm_x <- function(x){
    min_x <- quantile(na.omit(x),probs=0.01)
    x <- x - min_x
    max_x <- quantile(na.omit(x),probs=0.99)
    x / max_x
  }
  data[, x_rel := norm_x(x_rel), by=id]
}


ggetho(dt, aes(t, z=asleep)) +
  stat_tile_etho()

dt <- curate_data(dt)

ggetho(dt, aes(t, z=moving, y=paste(sex,id))) +
  stat_tile_etho()

ggetho(dt, aes(t, z=x_rel, y=paste(sex,id))) +
  stat_tile_etho()

pl <- ggetho(dt[t < days(1)], aes(t, y=x_rel, colour=sex), summary_time_window = 10) +
  geom_line() + facet_wrap( ~ id)


library(WaveletComp)
library(cowplot)

dt_target <- dt

#~ pdf("/tmp/test_wt.pdf", w=16, h=9)

#for(i in 1:nrow(dt_target[meta=T])){
#~ for(i in 1:10){
# i=67 good
#dtest <- dt_target[id == dt_target[i,id, meta=T], .(id, x_rel, max_velocity, moving, t)]

wt_analysis <- function(dd){
  min_period <- 10
  max_period <- hours(24)

  upper_period = ceiling( log2(max_period/ min_period))

  #upper_period = ceiling(1 + log2(max_period/ min_period))
  wt <- analyze.wavelet(dd,"x_rel",
                        loess.span = 0,
                        dj = 1/64,
                        dt = 1,
                        lowerPeriod = 2 ^ 1,
                        upperPeriod = 2 ^ upper_period,
                        make.pval =F,
                        verbose = T,
                        #n.sim=3
  )

  length(wt$axis.1)
  out <- data.table(power=wt$Power.avg, period = wt$Period * min_period)
  ggplot(out, aes(period, power)) + scale_x_log10(breaks=c(mins(2), hours(12), hours(24))) + geom_line()

  # brks <- c(mins(2), hours(12), hours(24))
  #~ 	a <- ggplot(dtest[t %between% hours(c(24, 48))], aes(x=t, y=x_rel)) + geom_line() + scale_x_hours() + stat_ld_annotations() +ggtitle(dt_target[i,id, meta=T])
  #~ 	b <- ggplot(out, aes(period, power)) + scale_x_log10(breaks=brks) + geom_line()
  #~ 	c <- ggplot(plot_dt, aes(x=x,y=y,fill=z)) + geom_raster() + scale_x_hours() +
  #~ 				scale_y_continuous(breaks = log2(brks/10)) +
  #~ 				stat_ld_annotations()

  #~ 	pl <- cowplot::plot_grid(a,cowplot::plot_grid(b,c, ncol=2),nrow=2)
  #~ 	print(pl)
  plot_dt
}



m <- cbind(expand.grid(wt$axis.2, wt$axis.1 * min_period), as.vector(wt$Power))
plot_dt <- as.data.table(m)
setnames(plot_dt, c("y", "x","z"))
dd
x <- dd$activity

cwt_spectrogram <- function(x,
                            period_range = c(hours(1), hours(32)),
                            sampling_rate = 1 / mins(1),
                            alpha = 0.05,
                            resolution = 1/64,
                            summary_time_window = mins(30)){
  signif_threshold = period = power = p_value = NULL
  min_period <- period_range[1]
  max_period <- period_range[2]
  upper_period = ceiling( log2(max_period* sampling_rate))
  lower_period = floor( log2(min_period* sampling_rate))

  # wt works with data frames
  dd <- data.table::data.table(x = x)
  #upper_period = ceiling(1 + log2(max_period/ min_period))
  wt <- WaveletComp::analyze.wavelet(dd,"x",
                                     loess.span = 0,
                                     dj = resolution,
                                     dt = 1,
                                     lowerPeriod = 2 ^ lower_period,
                                     upperPeriod = 2 ^ upper_period,
                                     make.pval = ifelse(n_sim >1,T,F),
                                     verbose = F,
                                     n.sim = 1
  )

  m <- cbind(expand.grid(wt$axis.2, wt$axis.1 / sampling_rate), as.vector(wt$Power))
  out <- as.data.table(m)
  setnames(out, c("period", "t","power"))
  out[, t :=  floor(t/summary_time_window) * summary_time_window]
  out[, period :=  2^period  / sampling_rate]
  out[, .(power=mean(power)), by="t,period"]
  #out <- out[, z := sqrt(z)]


}

y_res <- 100/ diff(range(out$period))

ggplot(out, aes(y=period, x=t,z=power)) + stat_summary_2d(binwidth = c(summary_time_window, y_res)) + scale_y_hours() +
  scale_fill_gradient2(limits=c(0, .4), high="red", low="blue", midpoint=.2) + scale_x_days() +stat_ld_annotations()

dev.off()

dt_target <- dt[id %in% dt_target[1:8 * 10,id, meta=T]]
dt_target <- dt
out <- dt_target[,wt_analysis(.SD), by=id]

brks <- c(mins(1), mins(2), hours(1), hours(12), hours(24))

pdf("/tmp/test_wt_selected.pdf", w=16, h=9)


selected = c(fem = "2016-07-29_14-58-07_009d6b|02", male = "2016-07-22_16-41-21_011d6b|11")

for(i in unique(out[id %in% selected , id, meta=T])){
  print(i)
  a <- ggplot(dt[id == i ], aes(x=t, y=x_rel)) +
    #~ 				geom_rect(xmin=hours(24), xmax=hours(25), ymax=Inf, ymin=-Inf, fill="green", inherit.aes = F) +
    #~ 				geom_rect(xmin=hours(30), xmax=hours(31), ymax=Inf, ymin=-Inf, fill="green", inherit.aes = F) +
    geom_line() + scale_x_days(limits=days(c(0, 5))) + scale_y_continuous(limits=c(0,1)) +
    stat_ld_annotations() + stat_summary_bin(fun.y = "mean", geom = "line", colour="blue", size=3, bins=12*5) +
    ggtitle(i)
  b <- ggplot(dt[id == i & t %between%  + hours(c(24, 25))], aes(x=t, y=x_rel)) + geom_line() + scale_x_hours() +
    stat_ld_annotations() + scale_y_continuous(limits=c(0,1))
  c <- ggplot(dt[id == i & t %between% + hours(c(30, 31))], aes(x=t, y=x_rel)) + geom_line() + scale_x_hours() +
    stat_ld_annotations() + scale_y_continuous(limits=c(0,1))
  #b <- ggplot(out[id==i], aes(period, power)) + scale_x_log10(breaks=brks) + geom_line()
  d <- ggplot(out[id==i][ , .(z=mean(z)), by=y], aes(y=z, x=y)) + geom_line() + coord_flip() +
    scale_x_continuous(breaks = log2(brks/10))   + scale_y_continuous(limits=c(0,1))

  e <- ggplot(out[id==i], aes(x=x,y=y,fill=z)) + geom_raster() + scale_x_hours() +
    scale_y_continuous(breaks = log2(brks/10)) +
    stat_ld_annotations()
  e <- e + scale_fill_gradient2(limits=c(0, 1.2), high="red", low="blue", midpoint=.5)
  ppp <- cowplot::plot_grid(e,d, ncol=2, rel_widths=c(3,1))

  pl <- cowplot::plot_grid(a,
                           cowplot::plot_grid(b,c,
                                              nrow=1), ppp,nrow=3)
  print(pl)
}


tmp_dt
out[ , .(z=mean(z)), by=y]
#out_bak <- copy(out)
out[, phase := ifelse(x < hours(12), "A", "B")]
tmp_dt <- rejoin(out[ , .SD[ , .(z=mean(z)), by="y,phase"], by=id])#[,.(z = mean(z)),by="y,sex"]


#tmp_dt <- rejoin(out[ , .SD[ , .(z=mean(z)), by=y], by=id])#[,.(z = mean(z)),by="y,sex"]

#~ dm <- ggplot(tmp_dt[sex=="M"], aes(y=z, x=y, linetype=phase, colour=sex)) + stat_pop_etho(method= mean_cl_boot) + coord_flip() +
#~ 		scale_x_continuous(breaks = log2(brks/10))   + scale_y_continuous(limits=c(0,1))

#~ df <- ggplot(tmp_dt[sex=="F"], aes(y=z, x=y)) + stat_pop_etho(method= mean_cl_boot) + coord_flip() +
#~ 		scale_x_continuous(breaks = log2(brks/10))   + scale_y_continuous(limits=c(0,1))


marg_pl <- ggplot(tmp_dt, aes(y=z, x=y,  colour=sex)) + stat_pop_etho(method= mean_cl_boot) + coord_flip(ylim=c(0,.6))+
  scale_x_continuous(breaks = log2(brks/10))  + facet_grid( . ~phase) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

sel_marg_pl <- ggplot(tmp_dt[id %in% selected], aes(y=z, x=y,  colour=sex)) + stat_pop_etho(method= mean_cl_boot) + coord_flip(ylim=c(0,.6))+
  scale_x_continuous(breaks = log2(brks/10))  + facet_grid( . ~phase) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )


pdf("/tmp/test_wt_marg.pdf", w=8, h=9)
marg_pl
sel_marg_pl
dev.off()



tmp_dt <- out[ xmv(sex)=="M", .(z=mean(z)), by="y,x"]

em <- ggplot(plot_dt, aes(x=x,y=y,fill=z)) + geom_raster() + scale_x_hours() +

  stat_ld_annotations()+
  scale_fill_gradient2(limits=c(0, 1.2), high="red", low="blue", midpoint=.5)
tmp_dt <- out[ xmv(sex)=="F", .(z=mean(z)), by="y,x"]
ef <- ggplot(tmp_dt, aes(x=x,y=y,fill=z)) + geom_raster() + scale_x_hours() +
  scale_y_continuous(breaks = log2(brks/10)) +
  stat_ld_annotations()+
  scale_fill_gradient2(limits=c(0, 1.2), high="red", low="blue", midpoint=.5)

pppm <- cowplot::plot_grid(em,dm, ncol=2, rel_widths=c(3,1))
pppf <- cowplot::plot_grid(ef,df, ncol=2, rel_widths=c(3,1))

pl <- cowplot::plot_grid(
  pppm, pppf,nrow=3)
print(pl)
dev.off()






#
ggplot(rejoin(spect_dt), aes(y=period, x=t,z=power)) +
    stat_tile_etho() + scale_y_log10() +
    facet_wrap(~ id) +
  scale_x_days() +
  stat_pop_etho(data= rejoin(spect_dt)[ridge==T], aes(t,period), colour="red", geom="point")

ggplot(rejoin(spect_dt), aes(y=period/hours(1), x=t,z=power)) +
  stat_tile_etho() + scale_y_log10() +
  facet_grid(id ~ .) +
  scale_x_days() +
  stat_pop_etho(data= rejoin(spect_dt)[ridge==T], aes(t,period/hours(1)), colour="red") +
  stat_ld_annotations()
