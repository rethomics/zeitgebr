context("periodogram")

test_that("periodrogram works in normal conditions", {
  data(dams_sample)
  pdt <- periodogram(activity, dams_sample, FUN=ls_periodogram, oversampling = 4)
  pdt <- rejoin(pdt)
  expect_equal(pdt[,mean(power),by=period_group][,V1], c(38.68601,23.71387,49.16648), tolerance=1e-5)
  
  pdt <- periodogram(activity, dams_sample, FUN=chi_sq_periodogram)
  pdt <- rejoin(pdt)
  expect_equal(pdt[,mean(power),by=period_group][,V1], c(1386.653,1332.233,1367.157), tolerance=1e-3)
  
  pdt <- periodogram(activity, dams_sample, FUN=fourier_periodogram)
  pdt <- rejoin(pdt)
  expect_equal(pdt[,mean(power),by=period_group][,V1], c(177.92472,56.70639,287.12976), tolerance=1e-5)
  
  # require(ggetho)
  # pl <- ggetho(dams_sample, aes(y = activity, colour = period_group)) +
  #   stat_pop_etho() + stat_ld_annotations(ld_colours = c("grey", "black")) +
  #   facet_grid( period_group ~ .)
  # pl
  #
  # pl_all <- ggplot(pdt, aes(period / hours(1), power, colour = period_group))  +
  #   geom_line() +
  #   geom_line(aes(y=signif_threshold), colour="grey", linetype="dashed") +
  #   facet_wrap( ~ id, labeller = id_labeller)
  # #
  # pl_all
  #
  #
  # expect_equal(nrow(unique(dt[,"id"])), 32)
  # expect_identical(dt[meta=TRUE]$region_id, 1:32)

})

