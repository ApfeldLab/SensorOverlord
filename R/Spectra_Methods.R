
#' A generic for the plotSpectra method
#'
#' @param object A spectra object
#' @param ... ...

#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotSpectra-generic
#'
#' @import ggplot2
setGeneric('plotSpectra', def = function(object, ...) standardGeneric("plotSpectra"))

#' Create a plot (ggplot object) for a sensorSpetra object
#'
#' Returns a ggplot object plotting the emission (relative)
#' as a function of wavelength (lambda) for a given sensorSpectra object
#'
#' @param object A sensorSpectra object
#' @param minimum_name The legend label for the state corresponding to Rmin
#' @param maximum_name The legend label for the state correspondign to Rmax
#'
#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotSpectra-sensorSpectra
setMethod('plotSpectra', "sensorSpectra", definition =
              function(object, minimum_name = "Minimum State", maximum_name = "Maximum State") {
                spectraData <- data.frame(lambda = object@lambdas, min = object@values_minimum, max = object@values_maximum)

                plot <- ggplot(spectraData) +
                    geom_line(aes(x = spectraData$lambda, y = spectraData$min, color = "#3C51C6")) +
                    geom_line(aes(x = spectraData$lambda, y = spectraData$max, color = "#B40426")) +
                    scale_color_manual(name = "Sensor State", labels = c(minimum_name, maximum_name),
                                       values = c("#3C51C6", "#B40426")) +
                    xlab(expression(lambda (nm)))  +
                    ylab("Fluorescent emission (relative)")

                return(plot)
              })
