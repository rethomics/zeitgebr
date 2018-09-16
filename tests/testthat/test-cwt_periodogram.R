context("cwt_perio")

test_that("wct_perio periodogram works", {
  data(dams_sample)
  dd <- dams_sample[id %in% dams_sample[meta=TRUE, ,id[1:5]]]
  per <- dd[,
                     cwt_periodogram(activity, sampling_rate = 1/300, n_sim = 1, resolution=1/32),
                     by = id]
  #ggetho::ggperio(per, aes(y = power, colour=id)) + geom_line()
})
