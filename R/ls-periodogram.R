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

  lsp_results <- lomb::lsp(x,
                           times = 1:length(x) / sampling_rate,
                           from = period_range[1],
                           to = period_range[2],
                           type="period",
                           alpha = alpha,
                           ofac= oversampling,
                           plot=FALSE)

  p_values <- exp(-lsp_results$power) *  lsp_results$n.out * 2 /  oversampling
  p_values <- ifelse(p_values >= 1, 1, p_values)
  out <- data.table::data.table(period = lsp_results$scanned,
                                power = lsp_results$power,
                                p_value = p_values,
                                signif_threshold = lsp_results$sig.level)
  out[power == lsp_results$peak]
  out
}




