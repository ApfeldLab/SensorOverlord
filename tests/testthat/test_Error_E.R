test_that("Spot-check error in E", {
    error <- Error_E(E = -275, Rmin = 1, Rmax = 5, delta = 0.2, e0 = -275, error_R = function(x) 0.02*x)

    expect_equal(error$E, -275)
    expect_lt(error$larger_E + 274.2516, 0.0001)
    expect_lt(error$smaller_E + 275.7789, 0.0001)
    expect_lt(error$max_error - 0.778922, 0.0001)
})
