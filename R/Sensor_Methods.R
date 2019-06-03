#' Get an array of R values (generic)
#'
#' This is just a simple method that generates a
#' range of values between Rmin and Rmax
#'
#' @param object A sensor object
#' @param ... ...
#'
#' @return A numeric array
#'
#' @family getR-methods
#' @export
setGeneric('getR', def = function(object, ...) standardGeneric("getR"))

#' Method to get an array of R values from a sensor
#'
#' Takes in a sensor object and returns a vector of valid R values
#'
#' @param object A sensor object
#' @param by The "by" argument for seq.
#' Specifies intervals between subsequent R values
#' @param edgeBy A multiplier for the precision of values near the edges
#' In the range (Rmin, Rmin + by) and (Rmax - by, Rmax), the by argument
#' will be divided by edgeBy.
#'
#' @return A numeric array between Rmin and Rmax
#'
#' @family getR-methods
#' @export
#' @docType methods
#'
#' @examples
#' my_sensor <- new("Sensor", Rmin = 1, Rmax = 5, delta = 0.5)
#' getR(my_sensor, by = 0.1)
setMethod('getR', "Sensor", definition =
              function(object, by = 0.01, edgeBy = 100) {
                  origBy <- by
                  minimum = min(object@Rmin, object@Rmax)
                  maximum = max(object@Rmin, object@Rmax)
                  return(
                      c(
                         seq(from = minimum,
                           to = minimum + origBy,
                           by = origBy/edgeBy),
                        seq(from = (minimum + (2 * origBy)),
                           to = (maximum - (2 * origBy)),
                           by = origBy),
                       seq(from = maximum - origBy,
                           to = maximum,
                           by = origBy/edgeBy))
                  )
              })


#' Generic for the getFractionMax method
#'
#' This method corresponds to finding the "OxD" in a redox sensor
#'
#' @param object A sensor object
#' @param ... ...
#'
#' @return A generic function for getFractionMax
#'
#' @export
setGeneric('getFractionMax', def = function(object, ...)
    standardGeneric("getFractionMax"))

#' Get the fraction of sensors in the state corresponding to "Rmax"
#'
#' This method corresponds to finding the "OxD" in a redox sensor
#'
#' @param object A sensor object
#' @param R A single number or numeric vector corresponding to an
#' R between Rmin and Rmax
#'
#' @return A single number of numeric vector of the fraction of sensors
#' in the maximum-emission state corresponding to the given R
#'
#' @export
#' @docType methods
#' @rdname getFractionMax-methods
#'
#' @examples
#' my_sensor <- new("Sensor", Rmin = 1, Rmax = 5, delta = 0.5)
#' getFractionMax(my_sensor, R = 1)
#' getFractionMax(my_sensor, R = 3)
#' getFractionMax(my_sensor, R = 5)
setMethod("getFractionMax", "Sensor", definition =
              function(object, R = getR(object)) {
                  return(
                      (R - object@Rmin) /
                          ((R - object@Rmin) + object@delta * (object@Rmax - R))
                  )
              })
#' Wrapper: Get the function that gets the biochemical property of this sensor
#'
#' @param object A sensor-type object
#' @param ... ...
#'
#' @return A function that gets the biochemical property of this sensor
#'
#' @export
#' @docType methods
setGeneric('getProperty', def = function(object, ...)
    standardGeneric("getProperty"))

#' Get the fraction of sensors in the state corresponding to "Rmax" (wrapper)
#'
#' @param object A sensor object
#' @param ... ...
#'
#' @return A single number of numeric vector of the fraction of sensors
#' in the maximum-emission state corresponding to the given R
#'
#' @export
#' @docType methods
#' @rdname getProperty-methods
#'
#' @examples
#' my_sensor <- new("Sensor", Rmin = 1, Rmax = 5, delta = 0.5)
#' getFractionMax(my_sensor, R = 1)
#' getFractionMax(my_sensor, R = 3)
#' getFractionMax(my_sensor, R = 5)
setMethod("getProperty", "Sensor", definition =
              function(object, ...) {
                  getFractionMax(object, ...)
              })

#' Get the redox potential (E) for a redox sensor (wrapper)
#'
#' For a given redox sensor at a certain temperature, returns the
#' redox potential corresponding to a given ratio (R) value
#'
#' @param object A redoxSensor object
#' @param ... ...
#'
#' @return A numeric array of E values
#'
#' @export
#' @docType methods
#' @rdname getProperty-redoxSensor
setMethod("getProperty", "redoxSensor", definition =
              function(object, ...) {
                    getE(object, ...)
              })

#' Get the pH of a pH sensor (wrapper)
#'
#' For a given redox sensor at a certain temperature, returns the
#' redox potential corresponding to a given ratio (R) value
#'
#' @param object A pHSensor object
#' @param ... ...
#' @return A numeric array of pH values
#'
#' @export
#' @docType methods
#' @rdname getProperty-pHSensor
setMethod("getProperty", "pHSensor", definition =
              function(object, ...) {
                    getpH(object, ...)
              })

#' Get the redox potential (E) value (generic)
#'
#' @param object An object
#' @param ... ...
#'
#' @return A numeric array of E values
#'
#' @export
#' @docType methods
setGeneric('getE', def = function(object, ...) standardGeneric("getE"))


#' Get the redox potential (E) for a redox sensor
#'
#' For a given redox sensor at a certain temperature, returns the
#' redox potential corresponding to a given ratio (R) value
#'
#' @param object A redoxSensor object
#' @param R (Optional, defaults to getR(Object)
#' A numeric value (can be an array) of ratio values
#' @param temp The temperature, in Kelvin. Default is 295.15
#'
#' @return A numeric array of E values
#'
#' @export
#' @docType methods
#' @rdname getE-redoxSensor
setMethod("getE", "redoxSensor", definition =
              function(object, R = getR(object), temp = 295.15) {
                      return(object@e0 - (8.315 * temp)/(2 * 96.48104) *
                                 log(
                                     (object@delta *
                                          object@Rmax - object@delta * R) /
                                         (R - object@Rmin)))
              })

#' Get the pH value (generic)
#'
#' @param object An object
#' @param ... ...
#'
#' @return A numeric array of pH values
#'
#' @export
#' @docType methods
setGeneric('getpH', def = function(object, ...) standardGeneric("getpH"))

#' Get the pH of a pH sensor
#'
#' For a given redox sensor at a certain temperature, returns the
#' redox potential corresponding to a given ratio (R) value
#'
#' @param object A pHSensor object
#' @param R (Optional, defaults to getR(Object)
#' A numeric value (can be an array) of ratio values
#'
#' @return A numeric array of pH values
#'
#' @export
#' @docType methods
#' @rdname getpH-pHSensor
setMethod("getpH", "pHSensor", definition =
              function(object, R = getR(object)) {
                  return(
                      object@pKa + log10(object@delta)
                      + log10((object@Rmax - R) / (R - object@Rmin))
                         )
              })



#' Get the the derivative of the redox potential (dE/dR) (generic)
#'
#' @param object An object
#' @param ... ...
#'
#' @return A numeric array of dE/dR values
#'
#' @export
#' @docType methods
setGeneric('getE_deriv', def = function(object, ...)
    standardGeneric("getE_deriv"))

#' Get the derivative of the redox potential (dE/dR) for a redox sensor
#'
#' For a given redox sensor at a certain temperature, returns the
#' derivative of the redox potential corresponding to a given ratio (R) value
#'
#' @param object A redoxSensor object
#' @param R (Optional, defaults to getR(Object)
#' A numeric value (can be an array) of ratio values
#'
#' @return A numeric array of dE/dR values
#'
#' @export
#' @docType methods
#' @rdname getE_deriv-redoxSensor
setMethod("getE_deriv", "redoxSensor", definition =
              function(object, R = getR(object)) {
                  return(
                      (-12.71*(object@Rmax - object@Rmin))/
                          ((R - object@Rmin) * (R - object@Rmax))
                         )
              })

#' Get the error (generic)
#'
#' @param object An object
#' @param ... ...
#'
#' @return A numeric array of errors
#'
#' @export
#' @docType methods
setGeneric('getAbsError', def = function(object, ...) standardGeneric("getAbsError"))

#' Get the error for a given sensor object
#'
#' Each R will have a certain error, defined by the Error_Model parameter
#' That error in R will propagate when applied to the FUN function parameter
#' This method returns that error
#'
#' @param object A sensor object
#' @param R An array of numeric ratio values
#' @param FUN A function that will be applied to R
#' @param Error_Model A function in the form Error_Model(R) --> Error in R
#' @param ... Extra parameters applied to the FUN function
#'
#' @return A numeric array of errors
#'
#' @export
#' @docType methods
#' @rdname getAbsError-sensor
setMethod("getAbsError", "Sensor", definition =
              function(object, R = getR(object), FUN = getProperty, Error_Model, ...) {
                  # Apply the R value to the error model
                  # Set an upper and lower R value based on the model
                  # (ASSUMES: symmetry in error in ratio e.g. that the
                  # true value of R between (R - error) and (R + error))
                  R_error <- Error_Model(R)
                  R_lower <- R + R_error
                  R_upper <- R - R_error

                  # Get the absolute difference in running the function between R and the R + error
                  # If you get an NA value, your error is infinite
                  property_error_higher <- suppressWarnings(FUN(object, R = R_upper, ...) - FUN(object, R = R, ...))
                  property_error_higher <- ifelse(test = is.na(property_error_higher), yes = Inf, no = abs(property_error_higher))

                  # Same thing, but with function between R and R - error
                  property_error_lower <- suppressWarnings(FUN(object, R = R_lower, ...) - FUN(object, R = R, ...))
                  property_error_lower <- ifelse(test = is.na(property_error_lower), yes = Inf, no = abs(property_error_lower))

                  # Get the maximum error
                  max_error <- pmax(property_error_lower, property_error_higher)

                  return(max_error)
              })

#' Get a table of the errors (generic)
#'
#' @param object An object
#' @param ... ...
#'
#' @return A dataframe of errors
#'
#' @export
#' @docType methods
setGeneric('getErrorTable', def = function(object, ...)
    standardGeneric("getErrorTable"))

#' Get the error table for a given sensor object
#'
#' Each R will have a certain error, defined by the Error_Model parameter
#' That error in R will propagate when applied to the FUN function parameter
#' This method returns (1) The input Rs (2) the error in Rs
#' (3) the results of the FUN function
#' parameter (4) the upper bound of the FUN function (5) the lower bound
#' of te fun function (6) the max absolute error for the FUN function
#'
#' @param object A sensor object
#' @param R An array of numeric ratio values
#' @param FUN A function that will be applied to R
#' @param Error_Model A function in the form Error_Model(R) --> Error in R
#' @param ... Extra parameters applied to the FUN function
#'
#' @return A dataframe of errors
#'
#' @export
#' @docType methods
#' @rdname getErrorTable-sensor
setMethod("getErrorTable", "Sensor", definition =
              function(object, R = getR(object), FUN = getProperty, Error_Model, ...) {

                  R_error <- Error_Model(R)

                  # What are your bounds of R, based on the error?
                  R_lower <- R - R_error
                  R_upper <- R + R_error

                  # Collect the values of your properties at the true R and the confidence bounds
                  property_true <- FUN(object, R, ...)

                  # If our upper_FUN larger than Rmax, you'll get an NA
                  # So you can convert that NA to Inf
                  property_tooHigh <- suppressWarnings(FUN(object, R_upper, ...))
                  property_tooHigh <- ifelse(test = is.na(property_tooHigh),
                                             yes = Inf, no = property_tooHigh)

                  property_tooLow <- suppressWarnings(FUN(object, R_lower, ...))
                  property_tooLow <- ifelse(test = is.na(property_tooLow),
                                            yes = Inf, no = property_tooLow)

                  # Take the differences between the properties obtained with the R bounds
                  # and the property obtain from the true R_individual value
                  property_error_higher <- abs(property_tooHigh - property_true)

                  property_error_lower <- abs(property_tooLow - property_true)

                  # Get & append the maximum error
                  property_error_max <- pmax(property_error_lower, property_error_higher)

                  return(data.frame(R = R, Error_R = R_error,
                                    FUN_true = property_true,
                                    upper_error = property_error_higher,
                                    lower_error = property_error_lower,
                                    max_abs_error = property_error_max))
              })


#' Plot the fraction of objects in the max state (generic)
#'
#' @param object An object
#' @param ... ...
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
setGeneric('plotFractionMax', def = function(object, ...) standardGeneric("plotFractionMax"))

#' Plot the fraction of sensors in the max state
#'
#' Creates a ggplot object that has the ratio R on the horizontal axis
#' and the fraction of sensors corresponding to the Rmax state on the vertical axis
#'
#' @param object An sensor object
#' @param FUN A function in the form FUN(Sensor, R) --> Fraction in Max State
#' @param R An array of numeric ratio values
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotFractionMax-Sensor
setMethod("plotFractionMax", "Sensor", definition =
              function(object, FUN = getFractionMax, R = getR(object)) {
                  R_Max <- data.frame(R = R, Max = FUN(object, R))

                  plot <- ggplot(R_Max) +
                      geom_line(aes(x = R_Max$R, y = R_Max$Max)) +
                      xlab("R")  +
                      ylab("Fraction in Max State")

                  return(plot)
              })

#' Plot the fraction of redox sensors in the max state
#'
#' Creates a ggplot object that has the ratio R on the horizontal axis
#' and the fraction oxidizied on the vertical axis
#'
#' @param object An redoxSensor object
#' @param R An array of numeric ratio values
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotFractionMax-redoxSensor
setMethod("plotFractionMax", "redoxSensor", definition =
              function(object, R = getR(object)) {
                  R_OXD <- data.frame(R = R, OXD = getFractionMax(object, R))

                  plot <- ggplot(R_OXD) +
                      geom_line(aes(x = R_OXD$R, y = R_OXD$OXD)) +
                      xlab("R")  +
                      ylab("Fraction Oxidized (OXD)")

                  return(plot)
              })

#' Plot the fraction of pH sensors in the max state
#'
#' Creates a ggplot object that has the ratio R on the horizontal axis
#' and the fraction deprotenated on the vertical axis
#'
#' @param object An pHSensor object
#' @param R An array of numeric ratio values
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotFractionMax-pHSensor
setMethod("plotFractionMax", "pHSensor", definition =
              function(object, R = getR(object)) {
                  R_deprot <- data.frame(R = R,
                                         deprot = getFractionMax(object, R))

                  plot <- ggplot(R_deprot) +
                      geom_line(aes(x = R_deprot$R, y = R_deprot$deprot)) +
                      xlab("R")  +
                      ylab("Fraction Deprotenated")

                  return(plot)
              })

#' Plot the fraction of E of an object
#'
#' @param object An object
#' @param ... ...
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
setGeneric('plotE', def = function(object, ...) standardGeneric("plotE"))

#' Plot the fraction of E of a redoxSensor
#'
#' Creates a ggplot object that has the ratio R on the horizontal axis
#' and the redox potential (E) on the vertical axis
#'
#' @param object An redoxSensor object
#' @param R An array of numeric ratio values
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotE-redoxSensor
setMethod("plotE", "redoxSensor", definition =
              function(object, R = getR(object)) {
                  R_E <- data.frame(R = R, E = getE(object, R))

                  plot <- ggplot(R_E) +
                      geom_line(aes(x = R_E$R, y = R_E$E)) +
                      xlab("R")  +
                      ylab("E_GSH (mV)")

                  return(plot)
              })

#' Plot the fraction of derivative of E of an object
#'
#' @param object An object
#' @param ... ...
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
setGeneric('plotE_deriv', def = function(object, ...) standardGeneric("plotE_deriv"))

#' Plot the derivative of E of a redoxSensor
#'
#' Creates a ggplot object that has the ratio R on the horizontal axis
#' and the derivative of redox potential (dE/dR) on the vertical axis
#'
#' @param object An redoxSensor object
#' @param R An array of numeric ratio values
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotE_deriv-redoxSensor
setMethod("plotE_deriv", "redoxSensor", definition =
              function(object, R = getR(object)) {
                  # Cut the first and last values of R, since those
                  # have an undefined derivitive
                  R <- R[2:length(R)-1]

                  R_E <- data.frame(R = R, E = getE_deriv(object, R))

                  plot <- ggplot(R_E) +
                      geom_line(aes(x = R_E$R, y = R_E$E)) +
                      xlab("R")  +
                      ylab("dE/dR (mV)")

                  return(plot)
              })
