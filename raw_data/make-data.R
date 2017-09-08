library(damr)
query <- data.frame(
  file = "dams_sample.txt",
  start_datetime = "2017-01-16 08:00:00",
  stop_datetime = "2017-01-26 00:09:00",
  region_id = 1:32,
  period_group = rep(c("long", "short", "wt"), c(10,11,11))
)

dams_sample <- damr::query_dam2(".", query)

ggetho::ggetho(dt, ggplot2::aes(y=activity))  +
  ggetho::stat_pop_etho() +
        ggplot2::facet_grid(period ~ .)

devtools::use_data(dams_sample, overwrite = TRUE)
