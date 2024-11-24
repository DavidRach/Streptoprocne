test_that("CivilTwilight returns a dataframe with more than 1 row", {
  # Prepare the experiment
  url <- "https://aa.usno.navy.mil/calculated/rstt/year?ID=AA&year=2024&task=2&lat=39.2805&lon=-76.5940&label=Baltimore%2C+MD&tz=5.00&tz_sign=-1&submit=Get+Data"

  # Execute the experiment
  TwilightTable <- CivilTwilight(x=url)

  # Did it return a data.frame
  expect_s3_class(TwilightTable, "data.frame")

  # Is it more than 1 row
  expect_gt(nrow(TwilightTable), 1)
})
