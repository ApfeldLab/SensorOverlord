#' @export
#' @import vdiffr

initSensor <- function() {
    return(new("Sensor", Rmin = 1,
               Rmax = 5,
               delta = 0.5))
}

sensor <- initSensor()
pHSensor <- new("pHSensor", sensor, pKa = 7)
redoxSensor <- new("redoxSensor", sensor, e0 = -265)

context("test-plots")

test_that("Plots have a known output", {
    # Fraction max plotting
    vdiffr::expect_doppelganger("Fraction Max, Generic",
                                plotFractionMax(object = sensor))
    vdiffr::expect_doppelganger("Fraction Max, pH",
                                plotFractionMax(object = pHSensor))
    vdiffr::expect_doppelganger("Fraction Max, redox",
                                plotFractionMax(object = redoxSensor))

    # Property plotting
    vdiffr::expect_doppelganger("Property, Generic",
                                plotProperty(object = sensor))
    vdiffr::expect_doppelganger("Property, pH",
                                plotProperty(object = pHSensor))
    vdiffr::expect_doppelganger("Property, redox",
                                plotProperty(object = redoxSensor))
})


test_that("Plotting spectra", {
    spectra <- spectraMatrixFromValues(
        lambdas_minimum <- 301:500,
        values_minimum <- c(seq(1, 100),
                            seq(301, 400)),
        lambdas_maximum <- 301:500,
        values_maximum <- c(seq(301, 400),
                            seq(1, 100))
    )

    vdiffr::expect_doppelganger("Plotting spectra",
                                plotSpectra(spectra))

})




