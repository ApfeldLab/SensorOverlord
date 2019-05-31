initSensor <- function() {
    return(new("Sensor", Rmin = runif(1, min = 1, max = 4),
               Rmax = runif(1, min = 5, max = 10),
               delta = runif(1, min = 0.01, max = 4)))
}

test_that("You can't get an E for a normal sensor", {
    sensor <- initSensor()
    expect_error(getE_deriv(sensor))
})

test_that("You can't get an E for a pH sensor", {
    sensor <- initSensor()
    pHSensor <- new("pHSensor", sensor, pKa = 7)
    expect_error(getE_deriv(pHSensor))
})

test_that("The minimum derivative is at the adjusted midpoint", {
    sensor <- initSensor()
    redoxSensor <- new("redoxSensor", sensor, e0 = -265)
    E <- getE(redoxSensor)
    E_deriv <- getE_deriv(redoxSensor)
    adjMidpoint <- redoxSensor@e0 - 12.71*log(redoxSensor@delta)

    expect_lt(E[match(min(E_deriv[is.finite(E_deriv)]), E_deriv)] -
                  adjMidpoint, 1)

    sensor <- initSensor()
    redoxSensor <- new("redoxSensor", sensor, e0 = -265)
    redoxSensor@delta = 1
    E <- getE(redoxSensor)
    E_deriv <- getE_deriv(redoxSensor)

    expect_lt(expect_lt(E[match(min(E_deriv[is.finite(E_deriv)]), E_deriv)] -
                                redoxSensor@e0, 1), 1)
})
