#' Computes periodograms
#'
#' This function builds peroidograms, with one of several methods, for each individual of [behavr] table
#'
#' @param var variable to analyse
#' @param data [behavr] table
#' @param period_range vector of size 2 defining minimal and maximal range of period to study (in seconds)
#' @param resample_rate frequency to resample (up or down) the data at (in hertz)
#' @param alpha  significance level
#' @param FUN  function used to compute periodogram (see [periodogram_methods])
#' @param ...  additional arguments to be passed to FUN
#' @return a [behavr] table with TODO
#'
#' @examples
#' data(dams_sample)
#' pdt <- periodogram(activity, dams_sample, FUN=ls_periodogram, oversampling = 4)
#' pdt <- periodogram(activity, dams_sample, FUN=chi_sq_periodogram)
#' @export
periodogram <- function(var,
                        data,
                        period_range = c(hours(16), hours(32)),
                        resample_rate = 1 / mins(15),
                        alpha = 0.01,
                        FUN = chi_sq_periodogram,
                        ...){
  var_of_interest <- deparse(substitute(var))
  regular_data <- resample(data, var_of_interest, resample_rate)

  data.table::setnames(regular_data, var_of_interest, "var__")

  reg_data_nval <- regular_data[, .(n_val = length(unique(var__))),
               by = c(data.table::key(regular_data))]

  invalid <- reg_data_nval[n_val < 2, id]
  if(length(invalid) > 0)
    warning(sprintf("Removing individuals that have only one unique value for `val`:\n%s",paste(invalid, sep="\n")))

  regular_data <- regular_data[!(id %in% invalid)]
  regular_data[, FUN(var__,
                     period_range = period_range,
                     sampling_rate = resample_rate,
                     alpha = alpha,
                     ...),
               by = c(data.table::key(data))]

}

#' helper unit-testable function
#' @noRd
resample <- function(data, var_of_interest, resample_rate){
  regular_data <- behavr::bin_apply_all(data,
                                        x = "t",
                                        y = var_of_interest,
                                        x_bin_length = 1 / resample_rate,
                                        string_xy = TRUE)
  f <- function(d){
    new_x <- seq(from = d[1, t], to = d[.N, t], by=1/resample_rate)
    out <- na.omit(as.data.table(approx(d[["t"]],y = d[[var_of_interest]], xout = new_x)))
    setnames(out, c("x", "y"), c("t",var_of_interest))
    out
  }

  regular_data <- regular_data[, f(.SD), by=id]
  regular_data
}
