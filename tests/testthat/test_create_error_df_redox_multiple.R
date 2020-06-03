test_that("Error table with two sensors has correct dimensions", {
  test_df <- create_error_df_redox_multiple(
    c(0.01, 0.02), -300, -200,
    data.frame(
      "Rmin" = c(1, 2),
      "Rmax" = c(5, 6),
      "delta" = c(0.2, 1.2),
      "name" = c("normal", "plusOne"),
      "e0" = c(-275, -274)
    )
  )

  expect_equal(all(dim(test_df) == c(40004, 6)), TRUE)
})

test_that("Error table with duplicate inaccuracies has correct dimensions", {
  params <- data.frame(name = "Sensor", Rmin = 0.852, Rmax = 6.65,
                                   delta = 0.171, e0 = -265)
  test_df <- create_error_df_redox_multiple(inaccuracies = c(0.018, 0.028, 0.018),
                                 Emin = -400, Emax = -200, param_df = params)
  expect_equal(all(dim(test_df) == c(60003, 6)), TRUE)
})
