library(damr)
metadata <- data.frame(
  file = "dams_sample.txt",
  start_datetime = "2017-01-16 08:00:00",
  stop_datetime = "2017-01-26 00:09:00",
  region_id = 1:32,
  period_group = rep(c("long", "short", "wt"), c(10,11,11))
)

metadata <- damr::link_dam2_metadata(metadata, ".")
dams_sample <- damr::load_dam2(metadata)


ggetho::ggetho(dams_sample, ggplot2::aes(y=activity))  +
  ggetho::stat_pop_etho() +
        ggplot2::facet_grid(period_group ~ .)

devtools::use_data(dams_sample, overwrite = TRUE)
