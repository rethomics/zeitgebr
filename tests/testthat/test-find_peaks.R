context("find_peaks")

test_that("find_peaks works in normal conditions", {
  data(dams_sample)
  pdt <- periodogram(activity, dams_sample, FUN=ac_periodogram)
  pdt <- find_peaks(pdt)
  expect_true(all(between(pdt[peak==1]$period / hours(1), 10, 30)))

#
  d <- copy(dams_sample)
  set.seed(134)
  d[, activity:= as.numeric(rnorm(.N) > 0)]
  pdt <- periodogram(activity, d, FUN=ac_periodogram, alpha = 1e-10)
  pdt <- find_peaks(pdt)


  expect_equal(nrow(pdt[peak==1]), 0)

})

#
#
#
#
# test_that("find_peaks core function works", {
#
#   y = c(1,1,1,1,2,3,4,4,4,1,1,2,3,4,1,2,3)
#   pval <- 1/(100 * y)
#   o <- zeitgebr:::findpeaks_pval(y,
#                             sortstr=TRUE,
#                             peakpat = "[+]{1,}[0]*[-]{1,}")
#
#   expect_identical(o, rbind(c(4,7,4,10), c(4,14,11,15)))
#
#   o <- zeitgebr:::findpeaks_pval(y,
#                             sortstr=TRUE)
#
#   expect_identical(o, matrix(c(4,14,11,15), nrow=1))
# })

