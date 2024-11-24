test_that("NOAA_Buouys returns a dataframe with more than 1 row", {
  # Prepare the experiment
  url <- "https://www.ndbc.noaa.gov/data/realtime2/BLTM2.txt"

  # Execute the experiment
  FtMcHenry <- NOAA_Buoys(x="https://www.ndbc.noaa.gov/data/realtime2/BLTM2.txt")

  # Did it return a data.frame
  expect_s3_class(FtMcHenry, "data.frame")

  # Is it more than 1 row
  expect_gt(nrow(FtMcHenry), 1)
})
