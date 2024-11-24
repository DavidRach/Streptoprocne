test_that("SunriseSunset returns a dataframe with more than 1 row", {
  # Prepare the experiment
  url <- "https://aa.usno.navy.mil/calculated/rstt/year?ID=AA&year=2024&task=0&lat=39.2805&lon=-76.5940&label=Baltimore%2C+MD&tz=5.00&tz_sign=-1&submit=Get+Data"

  # Execute the experiment
  SunriseTable <- Streptoprocne::SunriseSunset(x=url)

  # Did it return a data.frame
  expect_s3_class(SunriseTable, "data.frame")

  # Is it more than 1 row
  expect_gt(nrow(SunriseTable), 1)
})
