#' Get an array of R values from a sensor
#'
#' This is just a simple method that generates a range of values between Rmin and Rmax
#'
#' @param object A sensor object
#' @param by The "by" argument for seq. Specifies intervals between subsequent R values
#'
#' @return A numeric array between Rmin and Rmax
#'
#' @export
#' @docType methods
#' @rdname getR-methods
#'
#' @examples
#' my_sensor <- new("Sensor", Rmin = 1, Rmax = 5, delta = 0.5)
#' getR(my_sensor, by = 0.1)

setGeneric('getR', def = function(object, ...) standardGeneric("getR"))

setMethod('getR', "Sensor", definition =
              function(object, by = 0.01) {
                  return(seq(object@Rmin, object@Rmax, by = by))
              })

#' Get the fraction of sensors in the state corresponding to "Rmax"
#'
#' This method corresponds to finding the "OxD" in a redox sensor
#'
#' @param object A sensor object
#' @param R A single number or numeric vector corresponding to an R between Rmin and Rmax
#'
#' @return A single number of numeric vector of the fraction of sensors
#' in the maximum-emission state corresponding to the given R
#'
#' @export
#' @docType methods
#' @rdname getR-methods
#'
#' @examples
#' my_sensor <- new("Sensor", Rmin = 1, Rmax = 5, delta = 0.5)
#' getFractionMax(my_sensor, R = 1)
#' getFractionMax(my_sensor, R = 3)
#' getFractionMax(my_sensor, R = 5)

setGeneric('getFractionMax', def = function(object, ...) standardGeneric("getFractionMax"))

setMethod("getFractionMax", "Sensor", definition =
              function(object, R) {
                  return(
                      (R - object@Rmin) / ((R - object@Rmin) + object@delta * (object@Rmax - R))
                  )
              })

setGeneric('getE', def = function(object, ...) standardGeneric("getE"))

setMethod("getE", "redoxSensor", definition =
              function(object, R, temp = 295.15) {
                  return(
                      return(object@e0 - (8.315 * temp)/(2 * 96.48104) *
                                 log(
                                     (object@delta * object@Rmax - object@delta * R) /
                                         (R - object@Rmin)))
                  )
              })

setGeneric('getError', def = function(object, ...) standardGeneric("getError"))

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

