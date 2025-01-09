
test_that("calval_tracking has correct number of columns", {
  expect_equal(ncol(calval_tracking), 19)
})

test_that("deployment_can and retrieval_can are assigned correct tzone", {
  expect_equal(
    lubridate::tz(calval_tracking$deployment_can),"Canada/Atlantic")

  expect_equal(
    lubridate::tz(calval_tracking$retrieval_can), "Canada/Atlantic")

})

test_that("deployment_utc and retrieval_utc are assigned correct tzone", {
  expect_equal(lubridate::tz(calval_tracking$deployment_utc), "UTC")

  expect_equal(lubridate::tz(calval_tracking$retrieval_utc),"UTC")
})
