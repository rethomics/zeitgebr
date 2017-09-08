#' Fourier periodogram
#'
#' TODO
#'
#' @param x numeric vector
#' @param period_range vector of size 2 defining minimal and maximal range of period to study (in seconds)
#' @param sampling_rate the -- implicitly regular -- sampling rate of x (in hertz)
#' @param alpha  significance level
#' @return [data.table] ... todo
#' @examples
#' x <- behavr::toy_dam_data(duration = days(20))$activity
#' s <- fourier_periodogram(x,  period_range = c(hours(08), hours(18)))
#' @export
fourier_periodogram <- function(x,
                                period_range = c(hours(16), hours(32)),
                                sampling_rate = 1 / mins(1),
                                alpha = 0.05
                                          ){
  raw_periodo <- spec.pgram(x, detrend=TRUE, plot=FALSE)
  out <- data.table::data.table(power = raw_periodo$spec,
                                period = 1/ (raw_periodo$freq *sampling_rate) # frequence, in seconds
                                             )

  out <- out[ period %between% period_range]

  # peak period = argmax[period](power)
  peak_period <-out[power == max(power), period]

  # one should compute the pvalues directly
  out[, p_value := NA]

  # Compute the significance level with probablility 0.01, acordind to fisher 1929
  # signif_level is independent of period with this method
  out[, signif_level := - mean(power) * log((1 - (1 - alpha) ^ (1 / .N)))]
  out
}
