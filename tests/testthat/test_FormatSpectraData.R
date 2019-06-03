name <- "test_sensor"
type <- "redox_sensor"
readout <- "redox"
lambda_max <- 1:10
values_max <- 1:10
lambda_min <- 1:10
values_min <- 1:10
sensor_midpoint <- 1

test_that("Length validation works in formatting", {
    expect_error(formatSpectraData(
        name = c("name", "otherName"), type, readout, lambda_max,
        values_max, lambda_min, values_min, sensor_midpoint
    ), "Length of the name argument is not 1")

    expect_error(formatSpectraData(
        name, type = c("type", "otherType"), readout, lambda_max,
        values_max, lambda_min, values_min, sensor_midpoint
    ), "Length of the type argument is not 1")

    expect_error(formatSpectraData(
        name, type, readout = c("readout", "otherReadout"), lambda_max,
        values_max, lambda_min, values_min, sensor_midpoint
    ), "Length of the readout argument is not 1")

    expect_error(formatSpectraData(
        name, type, readout, lambda_max,
        values_max, lambda_min, values_min, sensor_midpoint = c(1, 2)
    ), "Length of the sensor_midpoint argument is not 1")

    expect_error(formatSpectraData(
        name, type, readout, lambda_max = 1:10,
        values_max = 1:20, lambda_min, values_min, sensor_midpoint
    ), "The lambda and values for the maximum state have
             different lengths")

    expect_error(formatSpectraData(
        name, type, readout, lambda_max,
        values_max, lambda_min = 1:10, values_min = 1:20, sensor_midpoint
    ), "The lambda and values for the minimum state have
             different lengths")
})

test_that("Type validation works in formatting", {
    expect_error(formatSpectraData(
        name = 2, type, readout, lambda_max,
        values_max, lambda_min, values_min, sensor_midpoint
    ), "The name argument must be a character type")

    expect_error(formatSpectraData(
        name, type = 2, readout, lambda_max,
        values_max, lambda_min, values_min, sensor_midpoint
    ), "The type argument must be a character type")

    expect_error(formatSpectraData(
        name, type, readout = 2, lambda_max,
        values_max, lambda_min, values_min, sensor_midpoint
    ), "The readout argument must be a character type")

    expect_error(formatSpectraData(
        name, type, readout, lambda_max = rep("", 10),
        values_max, lambda_min, values_min, sensor_midpoint
    ), "lambda_max must be numeric")

    expect_error(formatSpectraData(
        name, type, readout, lambda_max,
        values_max = rep("", 10), lambda_min, values_min, sensor_midpoint
    ), "values_max must be numeric")

    expect_error(formatSpectraData(
        name, type, readout, lambda_max,
        values_max, lambda_min = rep("", 10), values_min, sensor_midpoint
    ), "lambda_min must be numeric")

    expect_error(formatSpectraData(
        name, type, readout, lambda_max,
        values_max, lambda_min, values_min = rep("", 10), sensor_midpoint
    ), "values_min must be numeric")

    expect_error(formatSpectraData(
        name, type, readout, lambda_max,
        values_max, lambda_min, values_min, sensor_midpoint = ""
    ), "sensor_midpoint must be numeric")
})

context("test-formatspectradata")

test_that("A spectra can be adjusted properly, based on visual inspection", {
    spectra <- spectraMatrixFromValues(
        lambdas_minimum <- 301:500,
        values_minimum <- c(seq(1, 100),
                            seq(301, 400)),
        lambdas_maximum <- 301:500,
        values_maximum <- c(seq(301, 400),
                            seq(1, 100))
    )
    spectra_adjusted <- adjustSpectra(spectra, 0.95, 0.95)

    dfToPlot <- data.frame(lambda = c(spectra@lambdas,
                                      spectra_adjusted@lambdas),
                           values = c(spectra@values_minimum,
                                      spectra@values_maximum,
                                      spectra_adjusted@values_minimum,
                                      spectra_adjusted@values_maximum),
                           type = c(rep("Original", 3982),
                                    rep("Adjusted", 3982)))


    vdiffr::expect_doppelganger("spectra_adjustment",
                                ggplot(dfToPlot, aes(x = lambda, y = values, color = type)) +
                                    geom_point())
})
