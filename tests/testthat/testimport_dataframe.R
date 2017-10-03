context("import_dataframe")

test_that("data file exists", {
  file <- system.file("extdata/noaa_data_20170915.tsv", package = "earthquakes")
  expect_true(file.exists(file))
})

test_that("cleaning data file works", {
  file <- system.file("extdata/noaa_data_20170915.tsv", package = "earthquakes")
  raw_data <- eq_read_raw_data(file)
  df <- eq_clean_data(raw_data)
})
