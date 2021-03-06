initSensor <- function() {
    return(new("Sensor", Rmin = runif(1, min = 1, max = 4),
               Rmax = runif(1, min = 5, max = 10),
               delta = runif(1, min = 0.01, max = 4)))
}

initErrorModel <- function() {
    Error <- runif(1, min = 0.001, max = 0.5)
    function(x) {
        x * Error
    }
}


test_that("You can get errors from any sensor type", {
    sensor <- initSensor()
    redoxSensor <- new("redoxSensor", sensor, e0 = -265)
    pHSensor <- new("pHSensor", sensor, pKa = 7)
    error_model <- initErrorModel()

    expect_type(getAbsError(sensor, FUN = getProperty,
                              Error_Model = error_model),
                "double")
    expect_type(getAbsError(redoxSensor, FUN = getProperty,
                            Error_Model = error_model),
                "double")
    expect_type(getAbsError(pHSensor, FUN = getProperty,
                            Error_Model = error_model),
                "double")
})

test_that("You get the same number of errors as Rs", {
    sensor <- initSensor()
    Error_Model <- initErrorModel()

    expect_equal(length(getR(sensor)), length(getAbsError(sensor,
                                                          Error_Model = Error_Model)))
})

test_that("You errors are positive", {
    sensor <- initSensor()
    error_model <- initErrorModel()

    errors <- getAbsError(sensor, FUN = getProperty,
                          Error_Model = error_model)
    expect_true(all(errors >= 0))

})

test_that("If your error model returns 0, you get no errors", {
    sensor <- initSensor()
    error_model <- function(x){0}

    errors <- getAbsError(sensor, FUN = getProperty,
                          Error_Model = error_model)

    expect_true(all(errors == 0))

})

