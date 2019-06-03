lambdas_minimum <- seq(301, 600, by = 1)
lambdas_maximum <- seq(301, 600, by = 1)
values_minimum <- c(runif(150, min = 0, max = 1),
                    runif(150, min = 2, max = 3))
values_maximum <- c(runif(150, min = 2, max = 3),
                    runif(150, min = 0, max = 1))
spectra <- spectraMatrixFromValues(lambdas_minimum, values_minimum,
                                   lambdas_maximum, values_maximum)

test_that("Spectra maintain their identities but are lambda-dependent", {
    expect_true(identical(newSensorFromSpectra(spectra, c(340, 360), c(490, 510)),
                           newSensorFromSpectra(spectra, c(340, 360), c(490, 510))))

    expect_false(identical(newSensorFromSpectra(spectra, c(340, 360), c(490, 510)),
                     newSensorFromSpectra(spectra, c(320, 340), c(450, 470))))

})
