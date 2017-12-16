data(dams_sample)
dt <- copy(dams_sample)
######################
# detrend(activity, dams_sample, ...)
dt[, moving := activity > 0]
######################
library(ggetho)
ggetho(dt, aes(z = moving, y= interaction(id, period_group))) + stat_bar_tile_etho() + stat_ld_annotations()
ggetho(dt, aes(z = moving), multiplot = 2) + stat_bar_tile_etho() + facet_wrap( ~ id, ncol = 8, labeller = id_labeller)
ggetho(dt, aes(z = moving), multiplot = 2) + stat_tile_etho() + facet_wrap( ~ id, ncol = 8, labeller = id_labeller)
ggetho(dt, aes(z = moving), multiplot = 2) + stat_bar_tile_etho() + facet_grid(period_group ~ .)


## individual periodogram
per_dt_xs <- periodogram(moving, dt, FUN = chi_sq_periodogram)
per_dt_ls <- periodogram(moving, dt, FUN = ls_periodogram)
per_dt_ac <- periodogram(moving, dt, FUN = ac_periodogram)

ggetho::ggperio(per_dt_xs) + geom_line()  +
      geom_line(aes(y=signif_threshold), colour="blue") +
      facet_wrap( ~ id, ncol = 8, labeller = id_labeller)

ggetho::ggperio(per_dt_ls) + geom_line()  +
  geom_point(aes(colour = -log10(p_value))) +
  facet_wrap( ~ id, ncol = 8, labeller = id_labeller)


## average spectra
ggetho::ggperio(per_dt_xs[p_value < .05], aes(y= -log10(p_value), colour=period_group)) + stat_pop_etho(method=mean_cl_boot)
ggetho::ggperio(per_dt_xs[p_value < .05], aes(y= power - signif_threshold, colour=period_group)) + stat_pop_etho(method=mean_cl_boot)
ggetho::ggperio(per_dt_ls[p_value < .05], aes(colour=period_group)) + stat_pop_etho(method=mean_cl_boot)
ggetho::ggperio(per_dt_ac[p_value < .05], aes(colour=period_group)) + stat_pop_etho(method=mean_cl_boot)




ggetho::ggperio(per_dt_xs_with_peaks) + geom_line()  +
  geom_line(aes(y=signif_threshold), colour="blue") +
  geom_point(data = per_dt_xs_with_peaks[peak==1], col="red") +
  facet_wrap( ~ id, ncol = 8, labeller = id_labeller)

ggplot(rejoin(peaks), aes(y=peak_1, x=period_group)) + geom_jitter() + scale_y_time()




