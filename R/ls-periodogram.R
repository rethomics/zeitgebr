#' @param oversampling the oversampling factor
#' @rdname periodogram_methods
#' @export
ls_periodogram <- function(x,
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
                                #p_value = lps_results$p.value,
                                signif_threshold = lps_results$sig.level)
  #frecuency to hours.
  out
}


