context("find_peaks")

test_that("find_peaks works in normal conditions", {
  data(dams_sample)
  pdt <- periodogram(activity, dams_sample, FUN=ac_periodogram)
  pdt <- find_peaks(pdt)
  expect_true(all(between(pdt[peak==1]$period / hours(1), 10, 30)))


  d <- copy(dams_sample)
  set.seed(134)
  d[, activity:= as.numeric(rnorm(.N) > 0)]
  pdt <- periodogram(activity, d, FUN=ac_periodogram)
  pdt <- find_peaks(pdt, alpha=1e-10)

  expect_equal(nrow(pdt[peak==1]), 0)

})

