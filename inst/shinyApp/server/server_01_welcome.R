source(file.path("utils", "helpers.R"),  local = TRUE)

#Don't sanitize error messages
options(shiny.sanitize.errors = FALSE)

print("test")

# Global Functions -----------------------------------------------------
observeEvent(input$midpoint2, {
    updateNumericInput(session,
        inputId = "midpoint",
        value = input$midpoint2
    )
})

observeEvent(input$midpoint, {
    updateNumericInput(session,
                       inputId = "midpoint2",
                       value = input$midpoint
    )
})


# Get the minimum and maximum X values for the current sensor
getMinMax <- reactive({
    print("working!")
    # Set the precision with which to generate R values
    r_precision = 10^(input$rpres)
    r_precision_edge = 10^(input$rpres_edge)

    # Make a lookup table for the bounds associated with a sensor type
    boundsLookup <- list("redox" = c(-400,-100),
                         "pH" = c(1,14),
                         "other" = c(0,1))

    # Create a sensor object from the input sensor, if it's
    # from the database
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
                                 lambda_1 = c(405, 415),
                                 lambda_2 = c(465, 475))

        # Create the appropiate sensor
        sensor_type <- sensorData$sensor_type[[index]]
        sensor <- makeSpecificSensor(sensor, sensor_type,
                                     sensorData$sensor_midpoint[[index]])

        # Create the appropriate bounds
        bounds <- boundsLookup[[sensor_type]]
    }

    # Create a custom sensor
    else {

        # Make a sensor with custom characteristics
        sensor <- new("Sensor", Rmin = input$Rmin, Rmax = input$Rmax,
                      delta = input$delta)

        # Create a specific sensor object
        sensor <- makeSpecificSensor(sensor, input$sensorType,
                                     input$midpoint)

        # Create the appropriate bounds
        bounds <- boundsLookup[[input$sensorType]]

        # Pass the sensor type
        sensor_type <- input$sensorType
    }


    # Create an error model from input
    error_model <- function(x) {
        return(x*input$relErr + input$absErr)
    }

    # Create the error table for the sensor
    error_df <- getErrorTable(sensor, R = getR(sensor,
                                               by = r_precision, edgeBy = r_precision_edge),
                              FUN = getProperty, Error_Model = error_model)

    # Filter the error table on the desired accuracy
    error_filter <- subset(error_df, error_df$max_abs_error <= input$acc)

    # Get the minimum and maximum values measureable at the
    # desired accuracy
    minimum <- ifelse(test = length(error_filter$FUN_true) == 0,
                      yes = NaN, no = min(error_filter$FUN_true))
    maximum <- ifelse(test = length(error_filter$FUN_true) == 0,
                      yes = NaN, no = max(error_filter$FUN_true))

    # Create a data frame from the minimum and maximum values
    minMax <- data.frame(Minimum = c(round(minimum, 2)), Maximum = c(round(maximum,2)))

    # If no values are measureable --> throw an error
    if((is.nan(minMax$Minimum) || is.nan(minMax$Maximum))) {
        stop("No values satisfy the parameters.
You are trying to be impossibly accurate, given
your microscopy errors.")
    }

    # Reactively set the minimum and maximum plot bounds
    scale_factor <- (maximum-minimum)/3
    if(minimum - scale_factor < bounds[1]) {
        bounds[1] <- minimum - scale_factor
    }
    if(maximum + scale_factor > bounds[2]) {
        bounds[2] <- maximum + scale_factor
    }

    # Return the created data in a list
    return(list(minMax, bounds, error_df, sensor, sensor_type))
})


# Home Page -------------------------------------------------

# Output a dumbell plot of the ranges we can measure with
# this microscope precision and desired accuracy
output$range <- renderPlotly({

    minMax <- getMinMax()[[1]]
    bounds <- getMinMax()[[2]]
    error_df <- getMinMax()[[3]]

    plotBounds <- subset(error_df, error_df$FUN_true > minMax$Minimum &
                                    error_df$FUN_true < minMax$Maximum)

    ggplot(data = plotBounds, aes(x = FUN_true, y = 1)) +
        geom_line() +
        ylab("") +
        xlab("True Values") +
        scale_y_continuous(limits = c(.99, 1.01)) +
        theme(axis.ticks.y=element_blank(),
              axis.text.y = element_blank(),
              text = element_text(size = 16))
    # # Define a function that rounds to 0 decimal places
    # scaleFUN <- function(x) sprintf("%.0f", x)
    #
    # insert_minor <- function(major_labs, n_minor) {labs <-
    #     c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
    # labs[1:(length(labs)-n_minor)]}


    # g <- geom_linerange(data = minMax, aes(x = Sensor_Name, ymin = Minimum,
    #                                        ymax = Maximum, lwd = 1)) +
    #     ylab("Redox Potential") +
    #     xlab("") +
    #     scale_y_continuous(labels = scaleFUN, name = "", limits = bounds,
    #                        breaks = seq(bounds[1],
    #                                     bounds[2],
    #                                     by = (round(
    #                                         (bounds[2]-bounds[1])/
    #                                             14)) + 1)) +
    #     coord_flip()
    #
    # plot(g)


})

# Make a phase plot for the sensor
output$phasePlot <- renderPlotly({
    error_table <- getMinMax()[[3]]
    error_table <- error_table[Reduce(`&`, lapply(error_table, is.finite)),]

    ggplot(error_table, aes(x = FUN_true, y = max_abs_error)) +
        geom_line() +
        coord_flip() +
        theme(
            text = element_text(size = 16)
        ) +
        ylab("Accuracy") +
        xlab("True values") +
        ylim(c(0, input$acc * 4))


})

# Output a text version of the range we can measure
output$rangeText <- renderText({
    minMax <- getMinMax()[[1]]
    return(paste(minMax$Minimum, " to ", minMax$Maximum, sep = ""))
})

# Outputs the characteristics of the current sensor as text
output$sensorChars <- renderText({
    sensor <- getMinMax()[[4]]
    main <- paste("Rmin: ", round(sensor@Rmin, 2),
                  "| Rmax: ", round(sensor@Rmax, 2),
                  "| delta: ", round(sensor@delta, 2))

    sensor_type <- getMinMax()[[5]]

    midpoint <- switch(sensor_type, 'redox' = round(sensor@e0, 2),
                       'pH' = round(sensor@pH,2),
                       'other' = "NA")

    paste(main, "| Midpoint: ", midpoint)

})

# Custom Sensor Page ---------------------------------------------------
# Output the characteristics of the custom sensor
output$customChars <-output$customChars2 <- renderText({
    print(input)
    # Make a sensor with custom characteristics
    sensor <- new("Sensor", Rmin = input$Rmin, Rmax = input$Rmax,
                  delta = input$delta)

    # Create a specific sensor object
    sensor <- makeSpecificSensor(sensor, input$sensorType,
                                 input$midpoint)
    return(paste("Dynamic range: ", round(sensor@Rmax/sensor@Rmin,3)))
})

# Output the graph of R vs FractionMax of the custom sensor
output$plotFractionMax_custom <- output$plotFractionMax_custom2 <- renderPlot({
    # Make a sensor with custom characteristics
    sensor <- new("Sensor", Rmin = input$Rmin, Rmax = input$Rmax,
                  delta = input$delta)

    # Create a specific sensor object
    sensor <- makeSpecificSensor(sensor, input$sensorType,
                                 input$midpoint)
    return(plotFractionMax(sensor) +
               theme(aspect.ratio=1,
                     text = element_text(size = 20))
    )
})

# Output the graph of R vs Value for the custom sensor
output$plotValue_custom <- output$plotValue_custom2 <- renderPlot({
    # Make a sensor with custom characteristics
    sensor <- new("Sensor", Rmin = input$Rmin, Rmax = input$Rmax,
                  delta = input$delta)
    # Create a specific sensor object
    sensor <- makeSpecificSensor(sensor, input$sensorType,
                                 input$midpoint)

    R_Value <- data.frame(R = getR(sensor), Value = getProperty(sensor,
                                                                getR(sensor)))


    ggplot(R_Value, aes(x = R, y = Value)) +
        geom_line() +
        theme(aspect.ratio = 1,
              text = element_text(size = 20))

})

# Full Error Table Page ------------------------------------------------
output$fullTable <- renderDataTable({
    table <- getMinMax()[[3]]
    table$upper_error <- as.character(table$upper_error)
    table$lower_error <- as.character(table$lower_error)
    table$max_abs_error <- as.character(table$max_abs_error)
    colnames(table) <- c("R", "Error in R",
                         "True Value", "Upper Error",
                         "Lower Error", "Maximum Error")
    table
}, )
# Settings Page -------------------------------------------------------

# Render the number of R values we have generated
output$numR <- renderText({
    error_df <- getMinMax()[[3]]
    return(paste("With these settings, we have generated [",
                 length(error_df$R),
                 "] ratio intensity values", sep = " "))
})

# Render a histogram of the R values we have generated
output$numRHist <- renderPlot({
    error_df <- getMinMax()[[3]]
    hist(error_df$R, xlab = "R",
         main = "Distribution of generated R values")
})

# Render the maximum values that we can generate with this precision
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

# Render a histogram of the values we can generate with this precision
output$precisionHist <- renderPlot({
    error_df <- getMinMax()[[3]]
    Es <- subset(error_df$FUN_true, abs(error_df$FUN_true) < Inf)
    hist(Es, main = "Distribution of calculated values",
         xlab = "Values (noninfinite)")
})
