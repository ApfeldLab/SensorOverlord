lambdas_minimum <- seq(300, 600, by = 1)
lambdas_maximum <- seq(200, 550, by = 0.957)
values_minimum <- runif(200, min = 0, max = 1)
values_maximum <- runif(366, min = 0, max = 1)

test_that("Spectra only uses shared values from input", {
    spectra <- spectraMatrixFromValues(lambdas_minimum, values_minimum,
                                       lambdas_maximum, values_maximum)
    expect_lt(spectra@lambdas[1] - 300, 0.01)
    expect_lt(spectra@lambdas[length(spectra@lambdas)] - 550, 0.01)
})
