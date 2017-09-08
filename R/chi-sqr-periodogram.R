#' Chi Square Periodogram
#'
#' TODO
#'
#' @inheritParams fourier_periodogram
#' @param time_resolution the resolution of periods to scan
#' @examples
#' x <- behavr::toy_dam_data(duration = days(5))$activity
#' s <- chi_sq_periodogram(x)
#' \dontrun{
#' ggplot2::ggplot(s, ggplot2::aes(period, power)) +
#'         ggplot2::geom_line() +
#'         ggplot2::geom_line (ggplot2::aes(y = signif_level), linetype="dashed", colour="grey") +
#'         ggplot2::scale_x_time()
#'}
#' @seealso [xsp::chiSqPeriodogram] (code derived from)
#' @export
chi_sq_periodogram <- function(x,
                           period_range = c(hours(16), hours(32)),
                           sampling_rate = 1 / mins(1),
                           alpha = 0.05,
                           time_resolution = hours(0.1)){
  # lsp can handle time series, so lets use that feature!

  out <- data.table::data.table(
    period = seq(period_range[1],period_range[2],by=time_resolution)
  )
  out[ , power := sapply(period, calc_Qp, x, sampling_rate)]
  out[ , signif_level := qchisq((1 - alpha) ^ (1 / .N), round(period * sampling_rate))]
  out
}

#'calculate Qp
#'
#'@param values activity values (each value represents the measured activity in a minute)
#'@param varPer a period at which the chi-squared statistics is to be calculated
#'
#'@return a numeric of the calculated chi-squared statistics at the given varPer
#'@noRd
calc_Qp <- function(varPer, values, sampling_rate){
  col_num <- round(varPer * sampling_rate)

  row_num <- floor(length(values)/col_num)
  values_num <- row_num * col_num
  folded_values <- matrix(values[1 : values_num],
                          ncol = col_num,
                          byrow=T)


  avg_P <- colMeans(folded_values)
  avg_all <- mean(avg_P)
  numerator <- sum((avg_P - avg_all) ^ 2)
  denom <- sum((values - avg_all) ^ 2) / (values_num * row_num)
  qp <- numerator / denom
  qp
}
