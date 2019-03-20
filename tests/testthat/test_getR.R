test_that("Basic getR functionality works", {
    sensor <- new("Sensor", Rmin = 1, Rmax = 5, delta = 0.5)
    expect_equal(getR(sensor, by = 1), c(1, 2, 3, 4, 5))
})

test_that("Can pass a different 'by' argument", {
    sensor <- new("Sensor", Rmin = 1, Rmax = 5, delta = 0.5)
    expect_equal(getR(sensor, by = 0.5), seq(1, 5, by = 0.5))
})

test_that("getR works with redox sensors", {
    redoxSensor <- new("redoxSensor", Rmin = 1, Rmax = 5, delta = 0.5, e0 =-270)
    expect_equal(getR(redoxSensor, by = 1), c(1, 2, 3, 4, 5))
})

test_that("getR does not work with other data types", {
    expect_error(getR(list(Rmin = 1, Rmax = 5, delta = 1)))
})
