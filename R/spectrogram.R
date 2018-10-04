#' Computes spectrogram
#'
#' This function builds spectrogram, using CWT, for each individual of a [behavr] table
#'
#' @param var variable to analyse
#' @param data [behavr] table
#' @param period_range vector of size 2 defining minimal and maximal range of period to study (in seconds)
#' @param resample_rate frequency to resample (up or down) the data at (in hertz)
#' @param FUN  function used to compute spectrograms (so far, only CWT is implemented via [cwt_spectrogram])
#' @param ...  additional arguments to be passed to FUN
#' @return A [behavr::behavr] table.
#' In addition to the metadata, it contains data that encodes a spectrogram (i.e. power vs period).
#' The data contains the columns:
#' * `t` -- the time (in s) (same range the input time)
#' * `period` -- the period at which the `power` is computed, for a given `t` (in s)
#' * `power` -- the power the or equivalent (according to `FUN`)
#' * `ridge` -- a logical defining whether the point (`t` and `period`) is a ridge
#' @details A spectrogram is a estimation of the local periodicity of a signal at a given time.
#' In the context of circadian rhythm, it can be useful to understand how infradian rhythms change along the day or,
#' for instance, how circadian rhythm change ver the course of an multi-day experiment.
#' @examples
#' data(dams_sample)
#' dt <- dams_sample[id %in% dams_sample[meta=TRUE, ,id[1:5]]]
#' spect_dt <- spectrogram(activity, dt)
#'
#' \donttest{
#' require(ggetho)
#' ggspectro(spect_dt) +
#'         stat_tile_etho() +
#'         scale_y_log10() +
#'         facet_wrap(~ id)
#' }
#' @seealso
#' * [periodogram] -- to compute periodogram instead
#' * [cwt_spectrogram] -- The dunction use to compute individual spectrograms
#' * [ggetho::ggspectro] -- to plot spectrograms
#' @references
#' * [spectrogram tutorial](https://rethomics.github.io/ggetho.html#spectrograms) -- the relevant rehtomics tutorial
#' @export
spectrogram <- function(var,
                        data,
                        period_range = c(hours(16), hours(32)),
                        resample_rate = 1 / mins(15),
                        FUN = cwt_spectrogram,
                        ...){

  n_val = var__ = id = . = .N = t0 = .SD = NULL

  var_of_interest <- deparse(substitute(var))
  regular_data <- resample(data, var_of_interest, resample_rate)

  data.table::setnames(regular_data, var_of_interest, "var__")

  reg_data_nval <- regular_data[, .(n_val = length(unique(var__))),
                                by = c(data.table::key(regular_data))]

  invalid <- reg_data_nval[n_val < 2, id]
  if(length(invalid) > 0)
    warning(sprintf("Removing individuals that have only one unique value for `val`:\n%s",paste(invalid, sep="\n")))

  regular_data <- regular_data[!(id %in% invalid)]
  out <- regular_data[, FUN(var__,
                            period_range = period_range,
                            sampling_rate = resample_rate,
                            ...),
                      by = c(data.table::key(data))]

  time_origin <- data[, .(t0 = .SD[1,t]),by=id]
  out[, t:= out[time_origin, on="id"][,t +t0]]
  out
}

