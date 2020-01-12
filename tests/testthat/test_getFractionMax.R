initSensor <- function() {
    return(new("Sensor", Rmin = runif(1, min = 1, max = 4),
               Rmax = runif(1, min = 5, max = 10),
               delta = runif(1, min = 0.01, max = 4)))
}

test_that("Fraction max on a regular sensor ranges between 0 and 1", {
    sensor <- initSensor()
    fractionMax <- getFractionMax(sensor)

    expect_equal(fractionMax[1], 0)
    expect_equal(fractionMax[length(fractionMax)], 1)
})

test_that("Fraction max works for all sensor types", {
    sensor <- initSensor()
    max_sensor <- getFractionMax(sensor)
    max_redox <- getFractionMax(new("redoxSensor", sensor, e0 = -265))
    max_pH <- getFractionMax(new("pHSensor", sensor, pKa = 7))

    expect_equal(max_sensor, max_redox)
    expect_equal(max_sensor, max_pH)
})

test_that("If delta is 1, middle fraction max should be 0.5/50%, but
          not otherwise", {
    sensor <- initSensor()

    # Test with a delta of 0.5
    max_sensor <- getFractionMax(sensor)
    expect_false(median(max_sensor) == 0.5)

    # Test with a delta of 1
    sensor@delta = 1
    max_sensor <- getFractionMax(sensor)

    expect_lt(median(max_sensor) - 0.5, 0.01)
})
