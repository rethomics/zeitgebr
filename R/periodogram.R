#' Computes periodograms
#'
#' This function builds periodograms, with one of several methods, for each individual of a [behavr] table
#'
#' @param var variable to analyse
#' @param data [behavr] table
#' @param period_range vector of size 2 defining minimal and maximal range of period to study (in seconds)
#' @param resample_rate frequency to resample (up or down) the data at (in hertz)
#' @param alpha  significance level
#' @param FUN  function used to compute periodogram (see [periodogram_methods])
#' @param ...  additional arguments to be passed to FUN
#' @return A [behavr::behavr] table.
#' In addition to the metadata, it contains data that encodes a periodogram (i.e. power vs period).
#' The data contains the columns:
#' * `power` -- the power the or equivalent (according to `FUN`)
#' * `period` -- the period at which `power` is computed (in seconds)
#' * `p_value` -- the p value associated to the power estimation
#' * `signif threshold` -- the threshold above which power is considered significant
#'
#' @examples
#' data(dams_sample)
#' # only a half of the individuals for the sake of the example
#' dt <- dams_sample[xmv(region_id) %in% (1:16 * 2)]
#' pdt <- periodogram(activity, dt, FUN = ls_periodogram, oversampling = 4)
#' pdt <- periodogram(activity, dt, FUN = chi_sq_periodogram)
#' @seealso
#' * [periodogram_methods] -- the list of built-in methods
#' * [find_peaks] -- to find peaks in the periodogram
#' @references
#' * [zeitgebr tutorial](https://rethomics.github.io/zeitgebr.html) -- the relevant rehtomics tutorial

#' @export
periodogram <- function(var,
                        data,
                        period_range = c(hours(16), hours(32)),
                        resample_rate = 1 / mins(15),
                        alpha = 0.01,
                        FUN = chi_sq_periodogram,
                        ...){

  n_val = var__ = id = . = .N = NULL

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
  .N = id = .SD = NULL
  regular_data <- behavr::bin_apply_all(data,
                                        x = "t",
                                        y = var_of_interest,
                                        x_bin_length = 1 / resample_rate,
                                        string_xy = TRUE)
  f <- function(d){
    new_x <- seq(from = d[1, t], to = d[.N, t], by=1/resample_rate)
    out <- na.omit(data.table::as.data.table(approx(d[["t"]],y = d[[var_of_interest]], xout = new_x)))
    data.table::setnames(out, c("x", "y"), c("t",var_of_interest))
    out
  }

  regular_data <- regular_data[, f(.SD), by=id]
  regular_data
}
