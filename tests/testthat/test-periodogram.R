context("periodogram")

test_that("periodrogram works in normal conditions", {
  data(dams_sample)
  pdt <- periodogram(activity, dams_sample, FUN=ls_periodogram, oversampling = 4)
  pdt <- rejoin(pdt)
  expect_equal(pdt[,mean(power),by=period_group][,V1], c(38.68601,23.71387,49.16648), tolerance=1e-5)

  # pdt <- periodogram(activity, dams_sample, FUN=chi_sq_periodogram)
  # pdt <- rejoin(pdt)
  # expect_equal(pdt[,mean(power),by=period_group][,V1], c(1386.653,1332.233,1367.157), tolerance=1e-3)

  pdt <- periodogram(activity, dams_sample, FUN=fourier_periodogram)
  pdt <- rejoin(pdt)
  expect_equal(pdt[,mean(power),by=period_group][,V1], c(177.92472,56.70639,287.12976), tolerance=1e-5)

})


test_that("periodrogram warns when id have no data", {
  data(dams_sample)

  dams_sample2 <- copy(dams_sample)
  dams_sample2[id == "2017-01-16 08:00:00|dams_sample.txt|01", activity := 1]
  dams_sample2[id == "2017-01-16 08:00:00|dams_sample.txt|01"]

  reg_data_nval <- dams_sample2[, .(n_val =length(unique(activity))),
                                by = c(data.table::key(dams_sample2))]

  expect_warning(periodogram(activity, dams_sample2, FUN=chi_sq_periodogram),
                 "2017-01-16 08:00:00\\|dams_sample\\.txt\\|01")

})



