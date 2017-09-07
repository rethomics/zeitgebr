#' Lomb-Scargle Periodogram
#'
#' TODO
#'
#' @inheritParams fourier_periodogram
#' @param oversampling the oversampling factor
#' @examples
#' x <- behavr::toy_dam_data(duration = days(5))$activity
#' s <- lomb_periodogram(x,  period_range = c(hours(08), hours(18)))
#' # ggplot2::ggplot(s, ggplot2::aes(period, power)) + ggplot2::geom_line() + ggplot2::scale_x_time()
#' @seealso [lomb::lsp] the orginal function
#' @export
lomb_periodogram <- function(x,
                                period_range = c(hours(16), hours(32)),
                                sampling_rate = 1 / mins(1),
                                alpha = 0.05,
                                oversampling = 8
){
  # lsp can handle time series, so lets use that feature!

  lps_results <- lomb::lsp(x,
                           times = 1:length(x) / sampling_rate,
                           from = period_range[1],
                           to = period_range[2],
                           type="period",
                           alpha = alpha,
                           ofac= oversampling,
                           plot=FALSE)

  out <- data.table::data.table(period = lps_results$scanned,
                                power = lps_results$power,
                                p_value = lps_results$p.value,
                                signif_level = lps_results$sig.level)
  #frecuency to hours.
  out
}
