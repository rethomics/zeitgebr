context("periodogram")

test_that("periodrogram works in normal conditions", {
  data(dams_sample)
  pdt <- periodogram(activity, dams_sample, FUN=ls_periodogram, oversampling = 4)
  #pdt <- periodogram(activity, dams_sample, FUN=chi_sq_periodogram)
  pdt <- rejoin(pdt)

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

