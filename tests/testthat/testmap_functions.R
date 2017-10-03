context("map functions")

library(leaflet)
library(dplyr)
library(ggplot2)

test_that("timeline graph succeeds", {

  file <- system.file("extdata/noaa_data_20170915.tsv", package = "earthquakes")
  raw_data <- eq_read_raw_data(file)
  df <- eq_clean_data(raw_data)

  rf<- df %>% dplyr::filter(COUNTRY == "USA" | COUNTRY == "CHINA")
  ggplot(rf) +
    geom_timeline(aes(x = DATE, colour = TOTAL_DEATHS, size = EQ_PRIMARY),
                  alpha = 0.5, xmindate = 2000, xmaxdate = 2017) +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_line(colour = "grey", size = 0.5)) +
    labs(size = "Richter scale value ", colour = "# deaths ")
})


test_that("mapping succeeds", {
  df %>%
    filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "DATE")
})
