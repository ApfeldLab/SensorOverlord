initSensor <- function() {
    return(new("Sensor", Rmin = 1,
               Rmax = 5,
               delta = 0.5))
}

sensor <- initSensor()
pHSensor <- new("pHSensor", sensor, pKa = 7)
redoxSensor <- new("redoxSensor", sensor, e0 = -265)


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






