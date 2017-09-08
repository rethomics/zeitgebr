#' @import behavr
#' @importFrom data.table ":="
#' @importFrom data.table "%between%"
NULL
#' Computes periodograms
#'
#' This function builds peroidograms, with one of several methods, for each individual of [behavr] table
#' @param var variable to analyse
#' @param data [behavr] table
#' @param period_range vector of size 2 defining minimal and maximal range of period to study (in seconds)
#' @param resample_rate frequency to resample (up or down) the data at (in hertz)
#' @param alpha  significance level
#' @export
periodogram <- function(var,
                        data,
                        period_range = c(hours(16), hours(32)),
                        resample_rate = 1 / mins(1),
                        alpha = 0.05,
                        FUN = chi_sq_periodogram,
                        ...){
  var_of_interest <- deparse(substitute(var))
  regular_data <- behavr::bin_apply_all(data,
                                x = "t",
                                y = var_of_interest,
                                x_bin_length = 1 / resample_rate,
                                string_xy = TRUE)
  data.table::setnames(regular_data, var_of_interest, "var__")
  regular_data[, FUN(var__,
                     period_range = period_range,
                     sampling_rate = resample_rate,
                     alpha = alpha,
                     ...),
               by = c(data.table::key(data))]

}
