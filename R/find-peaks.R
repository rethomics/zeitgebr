#' Find peaks in a periodogram
#'
#' This function locates the peaks in a pregenerated periodogram.
#' Detection is based on [pracma::findpeaks].
#' Only the significant (i.e. `power > signif_threshold`) peaks are extracted.
#'
#' @param data [behavr::behavr] table representing a periodogram, as returned by [periodogram]
#' @param n_peaks maximal numbers of peak to be detected
#' @return [behavr::behavr] table that is `data` with an extra column `peak`.
#' `peak` is filled with zeros except for rows match a peak.
#' In which case, rows have an integer value corresponding to the rank of the peak (e.g. 1 for the first peak).
#' @examples
#' data(dams_sample)
#' # only a half of the individuals for the sake of the example
#' dt <- dams_sample[xmv(region_id) %in% (1:16 * 2)]
#' per_dt_xs <- periodogram(activity, dt, FUN = chi_sq_periodogram)
#' per_dt_xs_with_peaks <- find_peaks(per_dt_xs)
#' per_dt_xs_with_peaks[peak == 1]
#' @seealso
#' * [periodogram] -- to generate a periodogram in a first place
#' * [ggetho::geom_peak] -- a layer to show peaks on a periodogram
#' @references
#' * [zeitgebr tutorial](https://rethomics.github.io/zeitgebr.html) -- the relevant rehtomics tutorial
#' @export
find_peaks <-  function(data, n_peaks=3){
  .SD =  peak = NULL
  out <- data.table::copy(data)
  out[ , peak := find_peaks_wapped(.SD, n_peaks = n_peaks), by = c(data.table::key(out))]
  # peaks are 0, not NA
  out[, peak := ifelse(is.na(peak), 0L, peak)]
}

#' @noRd
find_peaks_wapped <- function(d, n_peaks = 3){
  signif_threshold = NULL
  x <- d[, power - signif_threshold]

  peak_mat <- pracma::findpeaks(x,
                                peakpat = "[+]{1,}[0]*[-]{1,}",
                                sortstr = TRUE,
                                minpeakheight = 0
                            )
  peak_idx <- rep(NA_real_,n_peaks)
  out <- rep(NA_integer_, nrow(d))
  if(!is.null(peak_mat))
    out[peak_mat[,2]] <-  1:nrow(peak_mat)
  out
}

# adapted from [pracma::findpeaks]
#' @noRd
findpeaks_pval <- function (x, #pval, #signif_threshold,
                            nups = 1, ndowns = nups, zero = "0", peakpat = "[+]{1,}[0]*[-]{1,}",
                            minpeakdistance = 1, threshold = 0,  npeaks = 0, sortstr = FALSE){
  stopifnot(is.vector(x, mode = "numeric") || length(is.na(x)) ==
              0)
  if (!zero %in% c("0", "+", "-"))
    stop("Argument 'zero' can only be '0', '+', or '-'.")
  xc <- paste(as.character(sign(diff(x))), collapse = "")
  xc <- gsub("1", "+", gsub("-1", "-", xc))
  if (zero != "0")
    xc <- gsub("0", zero, xc)
  if (is.null(peakpat)) {
    peakpat <- sprintf("[+]{%d,}[-]{%d,}", nups, ndowns)
  }
  rc <- gregexpr(peakpat, xc)[[1]]
  if (rc[1] < 0)
    return(NULL)
  x1 <- rc
  x2 <- rc + attr(rc, "match.length")
  attributes(x1) <- NULL
  attributes(x2) <- NULL
  n <- length(x1)
  xv <- xp <- numeric(n)
  for (i in 1:n) {
    xp[i] <- which.max(x[x1[i]:x2[i]]) + x1[i] - 1
    xv[i] <- x[xp[i]]
  }

  inds <- which( xv - pmax(x[x1], x[x2]) >=
                  threshold)
  X <- cbind(xv[inds], xp[inds], x1[inds], x2[inds])
  if (minpeakdistance < 1)
    warning("Handling 'minpeakdistance < 1' is logically not possible.")
  if (sortstr || minpeakdistance > 1) {
    sl <- sort.list(X[, 1], na.last = NA, decreasing = TRUE)
    X <- X[sl, , drop = FALSE]
  }
  if (length(X) == 0)
    return(c())
  if (minpeakdistance > 1) {
    no_peaks <- nrow(X)
    badpeaks <- rep(FALSE, no_peaks)
    for (i in 1:no_peaks) {
      ipos <- X[i, 2]
      if (!badpeaks[i]) {
        dpos <- abs(ipos - X[, 2])
        badpeaks <- badpeaks | (dpos > 0 & dpos < minpeakdistance)
      }
    }
    X <- X[!badpeaks, ]
  }
  if (npeaks > 0 && npeaks < nrow(X)) {
    X <- X[1:npeaks, , drop = FALSE]
  }
  return(X)
}
