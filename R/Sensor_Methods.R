#' Get an array of R values (generic)
#'
#' This is just a simple method that generates a range of values between Rmin and Rmax
#'
#' @param object A sensor object
#' @param ... ...
#'
#' @return A numeric array
#'
#' @export
#' @docType methods
#' @rdname getR-generic
#'

setGeneric('getR', def = function(object, ...) standardGeneric("getR"))

#' Method to get an array of R values from a sensor
#'
#' Takes in a sensor object and returns a vector of valid R values
#'
#' @param object A sensor object
#' @param by The "by" argument for seq. Specifies intervals between subsequent R values
#'
#' @return A numeric array between Rmin and Rmax
#'
#' @export
#' @docType methods
#' @rdname getR-method
#'
#' @examples
#' my_sensor <- new("Sensor", Rmin = 1, Rmax = 5, delta = 0.5)
#' getR(my_sensor, by = 0.1)
setMethod('getR', "Sensor", definition =
              function(object, by = 0.01) {
                  return(seq(object@Rmin, object@Rmax, by = by))
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
#' @rdname getFractionMax-generics
setGeneric('getFractionMax', def = function(object, ...) standardGeneric("getFractionMax"))

#' Get the fraction of sensors in the state corresponding to "Rmax"
#'
#' This method corresponds to finding the "OxD" in a redox sensor
#'
#' @param object A sensor object
#' @param R A single number or numeric vector corresponding to an R between Rmin and Rmax
#' @param ... ...
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
              function(object, R) {
                  return(
                      (R - object@Rmin) / ((R - object@Rmin) + object@delta * (object@Rmax - R))
                  )
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
#' @rdname getE-generic
setGeneric('getE', def = function(object, ...) standardGeneric("getE"))

#' Get the redox potential (E) for a redox sensor
#'
#' For a given redox sensor at a certain temperature, returns the
#' redox potential corresponding to a given ratio (R) value
#'
#' @param object A redoxSensor object
#' @param R A numeric value (can be an array) of ratio values
#' @param temp The temperature, in Kelvin. Default is 295.15
#'
#' @return A numeric array of E values
#'
#' @export
#' @docType methods
#' @rdname getE-redoxSensor
setMethod("getE", "redoxSensor", definition =
              function(object, R, temp = 295.15) {
                  return(
                      return(object@e0 - (8.315 * temp)/(2 * 96.48104) *
                                 log(
                                     (object@delta * object@Rmax - object@delta * R) /
                                         (R - object@Rmin)))
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
#' @rdname getError-generic
setGeneric('getError', def = function(object, ...) standardGeneric("getError"))

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
#' @rdname getError-sensor
setMethod("getError", "Sensor", definition =
              function(object, R, FUN, Error_Model, ...) {
                  answer <- c()
                  for (R_individual in R) {
                      R_error <- Error_Model(R_individual)
                      R_lower <- R_individual + R_error
                      R_upper <- R_individual - R_error


                      # Get the absolute difference in running the function between R and the R + error
                      # You may get an error, in which case the error is infinite
                      value_error_up <- suppressWarnings(FUN(object, (R_upper), ...) - FUN(object, (R_individual), ...))
                      value_error_up = ifelse(test = is.na(value_error_up), yes = Inf, no = abs(value_error_up))

                      # Same thing, but with function between R and R - error
                      value_error_down <- suppressWarnings(FUN(object, (R_lower), ...) - FUN(object, (R_individual), ...))
                      value_error_down = ifelse(test = is.na(value_error_down), yes = Inf, no = abs(value_error_down))

                      # Get the maximum error
                      max_error <- max(value_error_down, value_error_up)

                      # Append the maximum error to the answer
                      answer <- c(answer, max_error)

                      # Print stuff
                      print(paste(R_individual, R_lower, R_upper, value_error_up, value_error_down, max_error))
                  }

                  return(answer)
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
#' @rdname plotFractionMax-generic
setGeneric('plotFractionMax', def = function(object, ...) standardGeneric("plotFractionMax"))

#' Plot the fraction of sensors in the max state
#'
#' Creates a ggplot object that has the ratio R on the horizontal axis
#' and the fraction of sensors corresponding to the Rmax state on the vertical axis
#'
#' @param object An sensor object
#' @param FUN A function in the form FUN(Sensor, R) --> Fraction in Max State
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotFractionMax-Sensor
setMethod("plotFractionMax", "Sensor", definition =
              function(object, FUN) {
                  R <- getR(object)
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
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotFractionMax-redoxSensor
setMethod("plotFractionMax", "redoxSensor", definition =
              function(object) {
                  R <- getR(object)
                  R_OXD <- data.frame(R = R, OXD = getFractionMax(object, R))

                  plot <- ggplot(R_OXD) +
                      geom_line(aes(x = R_OXD$R, y = R_OXD$OXD)) +
                      xlab("R")  +
                      ylab("Fraction Oxidized (OXD)")

                  return(plot)
              })

#' Plot the fraction of E of an object
#'
#' @param object An object
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotE-generic
setGeneric('plotE', def = function(object) standardGeneric("plotE"))

#' Plot the fraction of E of a redoxSensor
#'
#' Creates a ggplot object that has the ratio R on the horizontal axis
#' and the redox potential (E) on the vertical axis
#'
#' @param object An redoxSensor object
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotE-redoxSensor
setMethod("plotE", "redoxSensor", definition =
              function(object) {
                  R <- getR(object)
                  R_E <- data.frame(R = R, E = getE(object, R))

                  plot <- ggplot(R_E) +
                      geom_line(aes(x = R_E$R, y = R_E$E)) +
                      xlab("R")  +
                      ylab("E_GSH (mV)")

                  return(plot)
              })
