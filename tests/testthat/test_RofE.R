test_that("RofE should be consistent with E", {
    expect_equal(R_of_E(
        E = E(R = 3, Rmin = 1, Rmax = 5, delta = 0.2, e0 = -275),
        Rmin = 1, Rmax = 5, delta = 0.2, e0 = -275),
        3)
})
