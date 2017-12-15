context("chi_sq")

test_that("chi-sq periodogram works", {
  data(dams_sample)

  per <- dams_sample[,
              chi_sq_periodogram(activity, sampling_rate = 1/60),
             by = id]


  p <- chi_sq_periodogram(rep(0 , 1e4), sampling_rate = 1/60)

})

