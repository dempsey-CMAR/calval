test_that("cv_assign_tolerance_flag() assigns correct flags", {
  expect_equal(unique(tol_do_1$qc_flag), ordered(1, levels =c(1, 4)))

  expect_equal(unique(tol_do_4$qc_flag), ordered(4, levels =c(1, 4)))

  expect_equal(unique(tol_temp_1$qc_flag), ordered(1, levels =c(1, 4)))

  expect_equal(unique(tol_temp_4$qc_flag), ordered(4, levels =c(1, 4)))

})
