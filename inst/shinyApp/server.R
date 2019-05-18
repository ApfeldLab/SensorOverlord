require(shiny)
require(shinydashboard)
require(sensorOverlord)
require(dplyr)
require(ggplot2)
require(ggalt)
require(mongolite)
source("helpers.R")

#' @import shinydashboard

# Define server logic required to draw a histogram
shinyServer(
    function(input, output, session) {

        # Get the minimum and maximum X values for the current sensor
        getMinMax <- reactive({
            # Set the precision in R actual value
            r_precision = 10^(input$rpres)
            r_precision_edge = 10^(input$rpres_edge)

            # Set the types of sensors
            sensorTypeToObject <- list("redox" = "redoxSensor",
                                       "pH" = "pHSensor")

            sensorTypeToMidpoint <- list("redox" = "e0",
                                         "pH" = "pKa")

            boundsLookup <- list("redox" = c(-400,-100),
                           "pH" = c(1,14),
                           "other" = c(0,1))

            # Update the sensor, if needed
            if(input$sensors %in% sensorNames) {
                index <- match(input$sensors, sensorData$sensor_name)
                spectra <- spectraMatrixFromValues(
                    lambdas_minimum = sensorData$lambda_min[[index]],
                    values_minimum = sensorData$values_min[[index]],
                    lambdas_maximum = sensorData$lambda_max[[index]],
                    values_maximum = sensorData$values_max[[index]])

                # Make a sensor from the spectra
                sensor <-
                    newSensorFromSpectra(spectra,
                                         lambda_1 = c(420, 440),
                                         lambda_2 = c(460, 480))

                # Create the appropiate sensor
                sensor_type <- sensorData$sensor_type[[index]]
                sensor <- makeSpecificSensor(sensor, sensor_type,
                                             sensorData$sensor_midpoint[[index]])

                bounds <- boundsLookup[[sensor_type]]
            }

            # Case: custom sensor selected
            else {
                # Make a sensor with custom characteristics
                sensor <- new("Sensor", Rmin = input$Rmin, Rmax = input$Rmax,
                              delta = input$delta)

                sensor <- makeSpecificSensor(sensor, input$sensorType,
                                             input$midpoint)

                bounds <- boundsLookup[[input$sensorType]]

                }


            # Define an error model
            error_model <- function(x) {
                return(x*input$relErr + input$absErr)
            }


            # Create the error table
            error_df <- getErrorTable(sensor, R = getR(sensor,
                                  by = r_precision, edgeBy = r_precision_edge),
                                  FUN = getProperty, Error_Model = error_model)

            error_filter <- subset(error_df, error_df$max_abs_error <= input$acc)

            minimum <- ifelse(test = length(error_filter$FUN_true) == 0,
                              yes = NaN, no = min(error_filter$FUN_true))

            maximum <- ifelse(test = length(error_filter$FUN_true) == 0,
                              yes = NaN, no = max(error_filter$FUN_true))

            minMax <- data.frame(Minimum = c(round(minimum, 2)), Maximum = c(round(maximum,2)))

            if((is.nan(minMax$Minimum) || is.nan(minMax$Maximum))) {
                stop("No values satisfy the parameters.
You are trying to be impossibly accurate, given
your microscopy errors.")
            }

            if(minimum < bounds[1]) {
                #bounds[2] = bounds[2] - 100 - (bounds[1]-minimum)
                bounds[1] = minimum - 100
            }

            if(maximum > bounds[2]) {
                #bounds[1] = bounds[1] + 100 + (maximum - bounds[2])
                bounds[2] = maximum + 100
            }



            return(list(minMax, bounds, error_df))
        })

        output$numR <- renderText({
            error_df <- getMinMax()[[3]]
            return(paste("With these settings, we have generated [",
                         length(error_df$R),
                         "] ratio intensity values", sep = " "))
        })

        output$numRHist <- renderPlot({
            error_df <- getMinMax()[[3]]
            hist(error_df$R, xlab = "R",
                 main = "Distribution of generated R values")
        })

        output$precision <- renderText({
            error_df <- getMinMax()[[3]]
            Es <- subset(error_df$FUN_true, abs(error_df$FUN_true) < Inf)



            min <- round(min(Es),0)
            max <- round(max(Es),0)

            return(paste("The maximum values this app can generate are [",
                         min, "] to [", max, "]. To increase this range, adjust the
                     values in the 'Generating ratiometric intensity values' section.",
                         sep = " "))
        })

        output$precisionHist <- renderPlot({
            error_df <- getMinMax()[[3]]
            Es <- subset(error_df$FUN_true, abs(error_df$FUN_true) < Inf)
            hist(Es, main = "Distribution of calculated values",
                 xlab = "Values (noninfinite)")
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

        }, height = 400, width = 800)
    }
)
