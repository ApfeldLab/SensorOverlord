#' An S4 class to represent the emission spectrum of a 2-state sensor
#'
#' @slot lambdas represents excitation wavelengths in nanometers
#' @slot values_minimum represents the relative emission values of the sensor state
#' corresponding to the lowest R value Rmin
#' @slot values_maximum represents the relative emission values of the sensor state
#' corresponding to the highest R value Rmax
#'
#' @export
#'
#' @import methods
setClass("sensorSpectra",
         slots =
             list(lambdas = "numeric",
                  values_minimum = "numeric",
                  values_maximum = "numeric"
             ))
