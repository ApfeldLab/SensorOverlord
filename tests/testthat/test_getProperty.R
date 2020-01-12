initSensor <- function() {
    return(new("Sensor", Rmin = runif(1, min = 1, max = 4),
               Rmax = runif(1, min = 5, max = 10),
               delta = runif(1, min = 0.01, max = 4)))
}

test_that("The property of a generic sensor is the same as the fraction max", {
    sensor <- initSensor()
    fractionMax <- getFractionMax(sensor)
    property <- getProperty(sensor)

    expect_equal(fractionMax, property)
})

test_that("The property of a redox sensor should range from +Inf to -Inf", {
    sensor <- initSensor()
    redoxSensor <- new("redoxSensor", sensor, e0 = -265)
    property <- getProperty(redoxSensor)

    expect_equal(property[1], -Inf)
    expect_equal(property[length(property)], Inf)
})

test_that("The redox median should be near E0-12.71log(delta)", {
    sensor <- initSensor()
    redoxSensor <- new("redoxSensor", sensor, e0 = -265)
    property <- getProperty(redoxSensor)
    adjMidpoint <- redoxSensor@e0 - 12.71*log(redoxSensor@delta)

    expect_lt(median(property) - adjMidpoint, 1)

    sensor <- initSensor()
    redoxSensor <- new("redoxSensor", sensor, e0 = -265)
    redoxSensor@delta = 1
    property <- getProperty(redoxSensor)

    expect_lt(median(property) - redoxSensor@e0, 1)
})

test_that("The property of a pH sensor should range from -Inf to +Inf", {
    sensor <- initSensor()
    pHSensor <- new("pHSensor", sensor, pKa = 7)
    property <- getProperty(pHSensor)

    expect_equal(property[1], Inf)
    expect_equal(property[length(property)], -Inf)
})

test_that("The pH median should be near pKa+log_10(delta)", {
    sensor <- initSensor()
    pHSensor <- new("pHSensor", sensor, pKa = 7)
    property <- getProperty(pHSensor)
    adjpKa <- pHSensor@pKa + log10(pHSensor@delta)

    expect_lt(median(property) - adjpKa, 0.1)

    sensor <- initSensor()
    pHSensor <- new("pHSensor", sensor, pKa = 7)
    pHSensor@delta = 1
    property <- getProperty(pHSensor)

    expect_lt(median(property) - pHSensor@pKa, 0.1)
})
