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
#' * `signif_threshold` -- the significance threshold of the power (at alpha)
#' @details
#' TODO
#' @seealso
#' * [lomb::lsp] the orginal function for `ls_periodogram`
#' * [xsp::chiSqPeriodogram] (code derived from)
#' @name periodogram_methods
NULL
