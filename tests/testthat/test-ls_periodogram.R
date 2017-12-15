context("ls_perio")

test_that("ls_perio periodogram works", {
  data(dams_sample)

  per <- dams_sample[,
                     ls_periodogram(activity, sampling_rate = 1/60),
                     by = id]
 # ggetho::ggperio(per, aes(y = power - signif_threshold, colour=period_group)) + stat_pop_etho()


})
