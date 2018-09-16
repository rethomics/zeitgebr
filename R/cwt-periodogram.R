#' @param n_sim the number of shuffling simulation to compute p-value (see [WaveletComp::analyze.wavelet])
#' @param resolution the period resolution of the CWT (i.e. the number of suboctaves)
#' @rdname periodogram_methods
#' @export

cwt_periodogram <- function(x,
                            period_range = c(hours(16), hours(32)),
                            sampling_rate = 1 / mins(1),
                            alpha = 0.05,
                            resolution = 1/512,
                            n_sim = 10
){
  signif_threshold = period = power = p_value = NULL
  min_period <- period_range[1]
  max_period <- period_range[2]

  upper_period = ceiling( log2(max_period* sampling_rate))
  lower_period = floor( log2(min_period* sampling_rate))

  # wt works with data frames
  dd <- data.table::data.table(x = x)
  #upper_period = ceiling(1 + log2(max_period/ min_period))
  wt <- WaveletComp::analyze.wavelet(dd,"x",
                                     loess.span = 0,
                                     dj = resolution,
                                     dt = 1,
                                     lowerPeriod = 2 ^ lower_period,
                                     upperPeriod = 2 ^ upper_period,
                                     make.pval = ifelse(n_sim >1,T,F),
                                     verbose = F,
                                     n.sim = n_sim,
                                     method = "shuffle"
  )

  out <- data.table::data.table(power=wt$Power.avg, period = wt$Period /sampling_rate,
                    p_value = ifelse(is.null(wt$Power.avg.pval),NA,wt$Power.avg.pval))
  out[, signif_threshold := ifelse(p_value > alpha, 0, power/1.1)]
  out[period > period_range[1] & period < period_range[2]]
}
