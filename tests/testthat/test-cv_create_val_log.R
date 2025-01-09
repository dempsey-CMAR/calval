test_that("cv_create_log() will give a warning if more than one val_id is provided", {
  expect_error(
    cv_create_log(
      path,
      tracking = calval_tracking[c(2, 26), ]
    ))
})
