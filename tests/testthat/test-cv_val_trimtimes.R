test_that("cv_val_trimtimes() identified first timestamp for start time and last timestamp for retrieval", {

  tt <- cv_val_trimtimes(df_tt)

  expect_equal(tt$deployment_utc, c(tstamp_do, tstamp_temp, tstamp_temp))

  expect_equal(
    tt$retrieval_utc,
    c(tstamp_do + lubridate::days(1) + lubridate::hours(2),
      tstamp_temp + lubridate::days(1) + lubridate::hours(1),
      tstamp_temp + lubridate::days(1) + lubridate::hours(1))
  )
})
