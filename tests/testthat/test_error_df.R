test_that("error_df works for redox sensors", {
  my_sensor <- new("redoxSensor", new("Sensor", Rmin = 1, Rmax = 5, delta = 0.2), e0 = -250)
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

})
