library(shiny)
library(shinydashboard)
library(sensorOverlord)
library(dplyr)
library(ggplot2)
library(ggalt)
library(mongolite)
source("helpers.R")


# Define server logic required to draw a histogram
shinyServer(
    function(input, output, session) {
        getMinMax <- reactive({
            # Set the precision in R actual value
            r_precision = 10^(input$rpres)
            r_precision_edge = 10^(input$rpres_edge)

            # Update the sensor, if needed
            if(input$sensors != "None") {
                data <- subset(sensorData, sensorData$Sensor == input$sensors)
                spectra <- spectraMatrixFromValues(
                    lambdas_minimum = data$Min_lambda,
                    values_minimum = data$Min_values,
                    lambdas_maximum = data$Max_lambda,
                    values_maximum = data$Max_values)
                #print(spectra)
                sensor <-
                    newSensorFromSpectra(spectra,
                                         lambda_1 = c(420, 440),
                                         lambda_2 = c(460, 480))

                updateNumericInput(session, inputId = 'Rmin',
                                   value = round(sensor@Rmin, 3))
                updateNumericInput(session, inputId = 'Rmax',
                                   value = round(sensor@Rmax, 3))
                updateNumericInput(session, inputId = 'delta',
                                   value = round(sensor@delta, 3))
            }

            sensor <- new("Sensor", Rmin = input$Rmin, Rmax = input$Rmax,
                          delta = input$delta)
            bounds <- c(0,1)

            error_model <- function(x) {
                return(x*input$relErr + input$absErr)
            }

            if(input$sensorType == 'redox') {

                sensor <- new("redoxSensor", sensor, e0 = input$e0)

                error_df <- getErrorTable(sensor, R = getR(sensor,
                                                           by = r_precision, edgeBy = r_precision_edge),
                                          FUN = getE, Error_Model = error_model)
                bounds <- c(-400, -100)

            }

            if(input$sensorType == 'pH') {
                sensor <- new("pHSensor", sensor, pKa = input$pKa)
                error_df <- getErrorTable(sensor, R = getR(sensor, by = r_precision, edgeBy = r_precision_edge),
                                          FUN = getpH, Error_Model = error_model)
                bounds <- c(1,14)

            }

            if(input$sensorType == 'other') {
                error_df <- getErrorTable(sensor, R = getR(sensor, r_precision, edgeBy = r_precision_edge),
                                          FUN = getFractionMax,
                                          Error_Model = error_model)
            }

            error_filter <- subset(error_df, error_df$max_abs_error <= input$acc)

            minimum <- ifelse(test = length(error_filter$FUN_true) == 0,
                              yes = NaN, no = min(error_filter$FUN_true))

            maximum <- ifelse(test = length(error_filter$FUN_true) == 0,
                              yes = NaN, no = max(error_filter$FUN_true))

            minMax <- data.frame(Minimum = c(round(minimum, 2)), Maximum = c(round(maximum,2)))

            if(minimum < bounds[1]) {
                #bounds[2] = bounds[2] - 100 - (bounds[1]-minimum)
                bounds[1] = minimum - 100
            }

            if(maximum > bounds[2]) {
                #bounds[1] = bounds[1] + 100 + (maximum - bounds[2])
                bounds[2] = maximum + 100
            }

            if((is.nan(minMax$Minimum) || is.nan(minMax$Maximum))) {
                stop(strwrap("No values satisfy the parameters.
                 You are trying to be impossibly accurate,
                 given your microscopy errors."))
            }

            return(list(minMax, bounds, error_df))
        })

        output$precision <- renderText({
            error_df <- getMinMax()[[3]]
            Es <- subset(error_df$FUN_true, abs(error_df$FUN_true) < Inf)



            min <- round(min(Es),0)
            max <- round(max(Es),0)

            return(paste("The maximum values this app can generate are",
                         min, "to", paste(max, ".", sep = ""), "to increase this range, adjust the
                     general R precision and the precision near the edges.",
                         sep = " "))
        })

        output$range <- renderPlot({

            minMax <- getMinMax()[[1]]
            bounds <- getMinMax()[[2]]

            # Define a function that rounds to 0 decimal places
            scaleFUN <- function(x) sprintf("%.0f", x)

            g <- ggplot(minMax, aes(x = Minimum, xend = Maximum, y = "")) +
                geom_dumbbell(size = 1.5) +
                geom_text(data = minMax, aes(x = Minimum, label = Minimum),
                          vjust = 0.3, hjust = 1.2, size = 8) +
                geom_text(data = minMax, aes(x = Maximum, label = Maximum),
                          vjust = 0.3, hjust = -0.2, size = 8) +
                labs(y=NULL) +
                scale_x_continuous(labels = scaleFUN, name = "", limits = bounds,
                                   breaks = seq(bounds[1],
                                                bounds[2],
                                                by = (round(
                                                    (bounds[2]-bounds[1])/
                                                        14)) + 1)) +
                theme(axis.text.x = element_text(size = 24),
                      plot.background=element_rect(fill="#f7f7f7"),
                      panel.background=element_rect(fill="#f7f7f7"),
                      panel.grid.minor=element_blank(),
                      panel.grid.major.y=element_blank(),
                      legend.position="top",
                      panel.border=element_blank())

            plot(g)

        })
    }
)
