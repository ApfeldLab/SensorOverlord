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

#' @export
setGeneric('getE', def = function(object, ...) standardGeneric("getE"))

#' @export
setMethod("getE", "redoxSensor", definition =
              function(object, R, temp = 295.15) {
                  return(
                      return(object@e0 - (8.315 * temp)/(2 * 96.48104) *
                                 log(
                                     (object@delta * object@Rmax - object@delta * R) /
                                         (R - object@Rmin)))
                  )
              })

#' @export
setGeneric('getError', def = function(object, ...) standardGeneric("getError"))

#' @export
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

#' @export
setGeneric('plotFractionMax', def = function(object) standardGeneric("plotFractionMax"))

#' @export
setMethod("plotFractionMax", "Sensor", definition =
              function(object) {
                  R <- getR(object)
                  R_OXD <- data.frame(R = R, OXD = getFractionMax(object, R))

                  plot <- ggplot(R_OXD) +
                      geom_line(aes(x = R_OXD$R, y = R_OXD$OXD)) +
                      xlab("R")  +
                      ylab("Fraction in Max State")

                  return(plot)
              })

#' @export
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

#' @export
setGeneric('plotE', def = function(object) standardGeneric("plotE"))

#' @export
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
