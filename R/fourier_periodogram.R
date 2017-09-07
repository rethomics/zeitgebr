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
#' s <- fourier_periodogram(x, period_range = c(hours(08), hours(18)))
#' s[, period := period/hours(1)]
#' # to plot:
#' # ggplot(s, aes(period, spec)) + geom_line()
#' @export
fourier_periodogram <- function(x,
                                period_range = c(hours(16), hours(32)),
                                sampling_rate = 1 / mins(1),
                                alpha = 0.05
                                          ){
  raw_periodo <- spec.pgram(x, detrend=TRUE, plot=FALSE)
  fourier_analysis <- data.table::data.table(spec = raw_periodo$spec,
                                             freq = raw_periodo$freq *sampling_rate # frequence, in seconds
                                             )
    fourier_analysis <- fourier_analysis[, period := 1 / freq]
  fourier_analysis <- fourier_analysis[ period %between% period_range]

  # peak period = argmax[period](spec)
  peak_period <-fourier_analysis[spec == max(spec), period]

  # Compute the significance level with probablility 0.01, acordind to fisher 1929
  # signif_level is independent of period with this method
  fourier_analysis[, signif_level := - mean(spec) * log((1 - (1 - alpha) ^ (1 / .N)))]
  return(fourier_analysis)
}
