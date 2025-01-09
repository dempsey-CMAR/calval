test_that("cv_round_timestamp() returns an error when timestamp_utc is not a datetime", {

  expect_error(
    cv_round_timestamps(
      data.frame(
        variable = "dissolved_oxygen_percent_saturation",
        timestamp_utc = "2024-01-09 11:21:00"
      ))
  )
})

test_that("cv_round_timestamp() returns correct timestamps using default ints", {

  expect_equal(
    cv_round_timestamps(df_round)$round_timestamp,
     as_datetime(
      c("2024-01-09 11:20:00",
        "2021-07-30 18:00:00",
        "2020-03-15 08:15:00",
        "2020-03-15 08:10:00"
        )
  ))
})


test_that("cv_round_timestamp() returns correct timestamps using custom ints", {

  expect_equal(
    cv_round_timestamps(
      df_round,
      do_percent_sat_int = "2 minutes",
      sal_int = "5 minutes",
      temp_int = "10 minutes" )$round_timestamp,
    as_datetime(
      c("2024-01-09 11:22:00",
        "2021-07-30 17:55:00",
        "2020-03-15 08:10:00",
        "2020-03-15 08:14:00"
      )
    ))
})


