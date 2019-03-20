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

setGeneric('getFractionMax', def = function(object, ...) standardGeneric("getFractionMax"))

setMethod("getFractionMax", "Sensor", definition =
              function(object, R) {
                  return(
                      (R - object@Rmin) / ((R - object@Rmin) + object@delta * (object@Rmax - R))
                  )
              })
