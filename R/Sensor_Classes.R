# Sensor classes

#' An S4 class to represent a 2-state sensor
#'
#' @slot Rmin To represent the ratio emission value R in the minimum state
#' @slot Rmax to represent the ratio emission value R in the maximum state
#' @slot delta To represent the ratio between emission in the maximum and
#' minimum states in the second wavelength of the ratio.
#'
#' @export
#' @import methods
setClass("Sensor",
         slots =
             list(Rmin = "numeric", Rmax = "numeric", delta = "numeric")
)

setValidity("Sensor",
            function(object) {
                if (object@Rmin >= object@Rmax)
                    "Rmin must be smaller than Rmax"
                if ((object@Rmin < 0) | (object@Rmax < 0) | (object@delta) < 0)
                    "All parameters must be positive"
                else
                    TRUE
            })

#' An S4 class to represent a 2-state redox sensor
#'
#' @slot Rmin To represent the ratio emission value R in the reduced state
#' @slot Rmax to represent the ratio emission value R in the oxidized state
#' @slot delta To represent the ratio between emission
#'  in the oxidized and reduced
#' states in the second wavelength of the ratio.
#' @slot e0 The midpoint potential of the redox sensor
#'
#' @export
setClass("redoxSensor",
         slots =
             list(e0 = "numeric"),
         contains = "Sensor"
)

#' An S4 class to represent a 2-state redox sensor
#'
#' @slot Rmin To represent the ratio emission value R in the protenated state
#' @slot Rmax to represent the ratio emission value R in the deprotenated state
#' @slot delta To represent the ratio between emission in the
#' deprotenated and protenated states in the second wavelength of the ratio.
#' @slot pKa The midpoint/pKa of the redox sensor
#'
#' @export
setClass("pHSensor",
         slots =
             list(pKa = "numeric"),
         contains = "Sensor")



