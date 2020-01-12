initSensor <- function() {
    return(new("Sensor", Rmin = runif(1, min = 1, max = 4),
               Rmax = runif(1, min = 5, max = 10),
               delta = runif(1, min = 0.01, max = 4)))
}

test_that("You can't get an pH for a normal sensor", {
    sensor <- initSensor()
    expect_error(getpH(sensor))
})

test_that("You can't get an pH for a redox sensor", {
    sensor <- initSensor()
    redoxSensor <- new("redoxSensor", sensor, e0 = -265)
    expect_error(getpH(redoxSensor))
})

test_that("The pH of a pH sensor should range from -Inf to +Inf", {
    sensor <- initSensor()
    pHSensor <- new("pHSensor", sensor, pKa = 7)
    pH <- getpH(pHSensor)

    expect_equal(pH[1], Inf)
    expect_equal(pH[length(pH)], -Inf)
})

test_that("The pH median should be near pKa+log_10(delta)", {
    sensor <- initSensor()
    pHSensor <- new("pHSensor", sensor, pKa = 7)
    pH <- getpH(pHSensor)
    adjpKa <- pHSensor@pKa + log10(pHSensor@delta)

    expect_lt(median(pH) - adjpKa, 0.1)

    sensor <- initSensor()
    pHSensor <- new("pHSensor", sensor, pKa = 7)
    pHSensor@delta = 1
    pH <- getpH(pHSensor)

    expect_lt(median(pH) - pHSensor@pKa, 1)
})

