context("chi_sq")

test_that("chi-sq periodogram works", {
  data(dams_sample)

  per <- dams_sample[,
              chi_sq_periodogram(activity, sampling_rate = 1/60),
             by = id]

})


