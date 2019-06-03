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

test_that("Generic sensor:
          The R from the error table is the same as the R from the getR method", {
    sensor <- initSensor()
    Error_Model <- initErrorModel()
    error_table <- getErrorTable(sensor, Error_Model = Error_Model)

    expect_equal(getR(sensor), error_table$R)
})

test_that("Generic sensor:
          The property from the error table is the same as the property from
          the getProperty method", {
    sensor <- initSensor()
    Error_Model <- initErrorModel()
    error_table <- getErrorTable(sensor, Error_Model = Error_Model)

    expect_equal(getProperty(sensor), error_table$FUN_true)

})

test_that("Generic sensor:
          The maximum absolute error from the error table is the same as the error from
          the getAbsError method", {
              sensor <- initSensor()
              Error_Model <- initErrorModel()
              error_table <- getErrorTable(sensor, Error_Model = Error_Model)
              absError <- getAbsError(sensor, Error_Model = Error_Model)

              expect_equal(absError,
                           error_table$max_abs_error)
          })

test_that("Generic sensor:
          The upper and lower errors from the error table are no larger than the values from
          the getAbsError method", {
              sensor <- initSensor()
              Error_Model <- initErrorModel()
              error_table <- getErrorTable(sensor, Error_Model = Error_Model)
              absError <- getAbsError(sensor, Error_Model = Error_Model)

              expect_true(all(error_table$upper_error <= absError))
              expect_true(all(error_table$lower_error <= absError))
          })
