#' Create a spectra plot
#'
#' This just generates a ggplot object of the emission spectra from a sensorSpectra object
#'
#' @param object A spectra object
#' @param ... ...

#' @return A ggplot object
#'
#' @export
#' @docType methods
#' @rdname plotSpectra-methods
#'
#' @import ggplot2
setGeneric('plotSpectra', def = function(object, ...) standardGeneric("plotSpectra"))

#' @export
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
