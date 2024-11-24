test_that("FortyEightHour returns a dataframe with more than 1 row", {
  # Prepare the experiment
  url <- "https://forecast.weather.gov/MapClick.php?lat=39.2736&lon=-76.6264&unit=0&lg=english&FcstType=digital"

  # Execute the experiment
  HourlyWeatherForecast <- FortyEightHour(url)

  # Did it return a data.frame
  expect_s3_class(HourlyWeatherForecast, "data.frame")

  # Is it more than 1 row
  expect_gt(nrow(HourlyWeatherForecast), 1)
})
