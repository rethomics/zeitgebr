#' Methods For Computing Periodograms
#'
#' These functions provides a series of methods to assess periodicity of circadian processes.
#'
#' @param x numeric vector
#' @param sampling_rate the -- implicitly regular -- sampling rate of x (in hertz)
#' @inheritParams periodogram
#' @return a [data.table] with the columns:
#' * `period` -- the period (in s)
#' * `power` -- the power (or equivalent) for a given period
#' * `p_value` -- the significance of the power
#' * `signif_threshold` -- the significance threshold of the power (at alpha)
#' @seealso
#' * [lomb::lsp]  -- the orginal function for `ls_periodogram`
#' * [xsp::chiSqPeriodogram] -- code modified from
#' * [stats::acf] -- the orginal function for `ac_periodogram`
#' @references
#' * [zeitgebr tutorial](https://rethomics.github.io/zeitgebr.html) -- the relevant rehtomics tutorial
#' @name periodogram_methods
NULL

