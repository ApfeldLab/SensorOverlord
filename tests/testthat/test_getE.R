initSensor <- function() {
    return(new("Sensor", Rmin = runif(1, min = 1, max = 4),
               Rmax = runif(1, min = 5, max = 10),
               delta = runif(1, min = 0.01, max = 4)))
}

test_that("You can't get an E for a normal sensor", {
    sensor <- initSensor()
    expect_error(getE(sensor))
})

test_that("You can't get an E for a pH sensor", {
    sensor <- initSensor()
    pHSensor <- new("pHSensor", sensor, pKa = 7)
    expect_error(getE(pHSensor))
})

test_that("The E of a redox sensor should range from +Inf to -Inf", {
    sensor <- initSensor()
    redoxSensor <- new("redoxSensor", sensor, e0 = -265)
    E <- getE(redoxSensor)

    expect_equal(E[1], -Inf)
    expect_equal(E[length(E)], Inf)
})

test_that("The redox median should be near E0-12.71log(delta)", {
    sensor <- initSensor()
    redoxSensor <- new("redoxSensor", sensor, e0 = -265)
    E <- getE(redoxSensor)
    adjMidpoint <- redoxSensor@e0 - 12.71*log(redoxSensor@delta)

    expect_lt(median(E) - adjMidpoint, 1)

    sensor <- initSensor()
    redoxSensor <- new("redoxSensor", sensor, e0 = -265)
    redoxSensor@delta = 1
    E <- getE(redoxSensor)

    expect_lt(median(E) - redoxSensor@e0, 1)
})
