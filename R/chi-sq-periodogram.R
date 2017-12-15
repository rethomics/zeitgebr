#' @param time_resolution the resolution of periods to scan
#' @rdname periodogram_methods
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
  out[ , signif_threshold := qchisq((1 - alpha) ^ (1 / .N), round(period * sampling_rate))]
  out
}

#' Calculate Qp
#'
#' @param values activity values (each value represents the measured activity in a minute)
#' @param target_period a period at which the chi-squared statistics is to be calculated
#'
#' @return a numeric of the calculated chi-squared statistics at the given varPer
#' @noRd
calc_Qp <- function(target_period, values, sampling_rate){
  col_num <- round(target_period * sampling_rate)
  #row_num <- ceiling(length(values) / col_num)
  row_num <- length(values) / col_num
  dt <- data.table::data.table( col = (0 : (length(values) -1) %% col_num) + 1,
                                #row = ceiling(1:length(values) / col_num),
                                values = values,
                                key = "col")
  avg_P <- dt[, .(avg_P = mean(values)),by=col]$avg_P
  avg_all <- mean(values)
  numerator <- sum((avg_P - avg_all) ^ 2) *  (nrow(dt) * row_num)

  denom <- sum((values - avg_all) ^ 2)
  numerator / denom
}
