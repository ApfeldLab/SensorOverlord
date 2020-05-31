test_that("error_df works for redox sensors", {
  my_sensor <- new("redoxSensor", new("Sensor", Rmin = 1, Rmax = 5, delta = 0.2), e0 = -250)
  test_ranges <- ranges_df(my_sensor)
  minimum <- test_ranges$Minimum
  maximum <- test_ranges$Maximum

  expect_equal(minimum[1], "NA")
  expect_lt(as.numeric(minimum[2]) + 256.99, 0.01)
  expect_lt(as.numeric(minimum[3]) + 264.86, 0.01)
  expect_lt(as.numeric(minimum[4]) + 269.44, 0.01)
  expect_lt(as.numeric(minimum[5]) + 272.67, 0.01)

  expect_equal(maximum[1], "NA")
  expect_lt(as.numeric(maximum[2]) + 222.08, 0.01)
  expect_lt(as.numeric(maximum[3]) + 214.32, 0.01)
  expect_lt(as.numeric(maximum[4]) + 209.77, 0.01)
  expect_lt(as.numeric(maximum[5]) + 206.56, 0.01)


})
