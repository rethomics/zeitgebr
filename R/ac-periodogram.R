#' @rdname periodogram_methods
#' @export
ac_periodogram <- function(x,
                           period_range = c(hours(16), hours(32)),
                           sampling_rate = 1 / mins(1),
                           alpha = 0.05
){
  signif_threshold = period = power = p_value = NULL
  max_lag <- period_range[2] * sampling_rate
  min_lag <- period_range[1] * sampling_rate
  res <- acf(x, lag.max = max_lag, plot = F)
  clim <-  qnorm((1 -alpha))/sqrt(res$n.used)



  out <- data.table::data.table(period = res$lag[min_lag:length(res$lag)] /sampling_rate,
                     power = res$acf[min_lag:length(res$lag)],
                     signif_threshold = clim
                  )

  out[, p_value := 1 - pnorm(power * sqrt(res$n.used))]
  out
}
