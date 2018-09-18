#' Computes a spectrogram using CWT
#'
#' A port of Continuous Wavelet transform to `rethomics`.
#' This function is intended to be used as an argument in the [spectrogram] wrapper.
#' @inheritParams cwt_periodogram
#' @param summary_time_window the sampling period after post-processing.
#' Values of power are avegraged over this time window, for each period.
#' @seealso
#' * [spectrogram] -- to apply this fucntion to all indivvidual, with some preprocessing.
#' * [WaveletComp::analyze.wavelet] -- the orginal function for `cwt_spectrogram`
#' @export
cwt_spectrogram <- function(x,
                            period_range = c(hours(1), hours(32)),
                            sampling_rate = 1 / mins(1),
                            resolution = 1/64,
                            summary_time_window = mins(30)){

  signif_threshold = period = power = p_value = NULL
  ridge = period_at_ridge = . = NULL

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
                                     make.pval = FALSE,
                                     verbose = FALSE,
                                     n.sim = 1
  )

  m <- cbind(expand.grid(wt$axis.2, wt$axis.1 / sampling_rate), as.vector(wt$Power), as.integer(as.vector(wt$Ridge)))
  out <- data.table::as.data.table(m)
  data.table::setnames(out, c("period", "t","power", "ridge"))
  out[, t :=  floor(t/summary_time_window) * summary_time_window]
  out[, period :=  2^period  / sampling_rate]
  out[, .(power=mean(power), ridge=(sum(ridge))), by="t,period"]
  ridge_period_dt <- out[,  .(period_at_ridge = ifelse(max(ridge) > 0, period[which.max(ridge)],NA_real_)), by="t"]

  out <- ridge_period_dt[out, on=c("t")]
  out[,  ridge := ridge & as.integer(!is.na(period_at_ridge) & period_at_ridge == period)]
  out[, period_at_ridge := NULL]
  out[, ridge :=  ifelse(period == min(period), F, ridge)]
}


