initSensor <- function() {
    return(new("Sensor", Rmin = runif(1, min = 1, max = 4),
               Rmax = runif(1, min = 5, max = 10),
               delta = runif(1, min = 0.01, max = 4)))
}

test_that("E should range from +Inf to -Inf", {
    expect_equal(E(R = 1, Rmin = 1, Rmax = 5, delta = 0.2, e0 = -275), -Inf)
    expect_equal(E(R = 5, Rmin = 1, Rmax = 5, delta = 0.2, e0 = -275), Inf)
})
