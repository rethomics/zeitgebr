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





ggplot(rejoin(peaks), aes(y=peak_1, x=period_group)) + geom_jitter() + scale_y_time()




library(ggplot2)

geom_peak <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = TRUE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       peaks = 1,
                      conversion = hours) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPeak,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      peaks=peaks,
      conversion = conversion,
      ...
    )
  )
}


GeomPeak <- ggproto("GeomPeak", GeomPoint,
                     required_aes = c("x", "y", "peak"),
                     non_missing_aes = c("size", "shape", "colour"),
                     default_aes = aes(
                       shape = 4, colour = "blue", size = 3, fill = NA,
                       alpha = NA, stroke = 0.5,  angle = 0, hjust = .5,
                       vjust = 0, family = "", fontface = 1, lineheight = 1.2
                     ),

                     draw_panel = function(data, panel_params, coord, na.rm = FALSE, peaks = 1, conversion=hours) {
                       data <- data[data$peak %in% peaks, ]
                       if(nrow(data) < 1)
                         return(NULL)
                       coords <- coord$transform(data, panel_params)
                       label <- sprintf( "%.2f",data$x / conversion(1))
                       #ggplot2:::ggname("geom_point",
                       grid::gList(
                              grid::pointsGrob(
                                coords$x, coords$y,
                                pch = coords$shape,
                                gp = grid::gpar(
                                  col = alpha(coords$colour, coords$alpha),
                                  fill = alpha(coords$fill, coords$alpha),
                                  # Stroke is added around the outside of the point
                                  fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                  lwd = coords$stroke * .stroke / 2
                                )
                              ),
                              grid::textGrob(
                                label,
                                coords$x,
                                unit(0, "npc"),
                                hjust = data$hjust, vjust = data$vjust,
                                rot = data$angle,
                                gp = grid::gpar(
                                  col = alpha(data$colour, data$alpha),
                                  fontsize = data$size * .pt,
                                  fontfamily = data$family,
                                  fontface = data$fontface,
                                  lineheight = data$lineheight
                                )
                              )
                       )
                     },

                     draw_key = draw_key_point
)


ggetho::ggperio(per_dt_xs_with_peaks, aes(peak=peak)) + geom_line()  +
  geom_line(aes(y=signif_threshold), colour="red") +
  geom_peak(peaks = 1, colour="blue") +
  facet_wrap( ~ id, ncol = 8, labeller = id_labeller)


