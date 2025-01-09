test_that("cv_import_file_path() creates expected path", {

  expect_equal(
    cv_import_path("VAL0012", check_path = FALSE),
    "R:/data_branches/water_quality/validation/validation_data/val0012"
  )

  expect_equal(
    cv_import_path("POST0024", check_path = FALSE),
    "R:/data_branches/water_quality/validation/validation_data/post0024"
  )
})

test_that("cv_import_file_path() generates error if path does not exist", {

  expect_error(cv_import_path("missing_folder", check_path = TRUE))

})
