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



