test_that("Ranges are reasonable: test from roGFP2 HansonThesis", {
  error_df <- create_error_df_redox_multiple(
    c(0.02), -400, -200,
    data.frame(
      Rmin = 0.97,
      Rmax = 4.12,
      delta = 0.23,
      name = "roGFP2",
      e0 = -299
    )
  )
  expect_equal(dim(error_df)[1], 20001)
  ranges <- create_ranges_multiple(error_df)

  expect_lt(as.numeric(ranges$Minimum[2]) + 304, 1)
  expect_lt(as.numeric(ranges$Minimum[3]) + 313, 1)
  expect_lt(as.numeric(ranges$Minimum[4]) + 317, 1)
  expect_lt(as.numeric(ranges$Minimum[5]) + 321, 1)

  expect_lt(as.numeric(ranges$Maximum[2]) + 274, 1)
  expect_lt(as.numeric(ranges$Maximum[3]) + 266, 1)
  expect_lt(as.numeric(ranges$Maximum[4]) + 261, 1)
  expect_lt(as.numeric(ranges$Maximum[5]) + 258, 1)

})

test_that("Multiple inaccuracies are not excluded from a ranges df", {
  my_sensor <- data.frame(name = "Sensor", Rmin = 0.852, Rmax = 6.65,
                          delta = 0.171, e0 = -265)
  ranges <- create_ranges_multiple(
    error_df = create_error_df_redox_multiple(inaccuracies = c(0.018, 0.028, 0.018),
                                             Emin = -400, Emax = -200, param_df = my_sensor),
    thresholds=c(1)
  )

  expect_equal(nrow(ranges), 3)
})
