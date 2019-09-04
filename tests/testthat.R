library(testthat)
library(sensorOverlord)

if (Sys.getenv("USER") != "travis") {
    test_check("sensorOverlord")
}


