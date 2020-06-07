# Don't sanitize error messages
options(shiny.sanitize.errors = FALSE)

# Make a lookup table for the bounds associated with a sensor type
boundsLookup <- list(
    "redox" = c(-400, -100, 1),
    "pH" = c(1, 14, 0.1),
    "other" = c(0, 1, 0.01)
)

# Tab Links
observeEvent(input$link_to_analysis, {
  updateTabsetPanel(session, "Sensoroverlord", "analysis")
})
# Global Functions -----------------------------------------------------

# Gets the current sensor
sensorInfo <- reactiveValues()

# Update sensor
observeEvent(input$`use-custom-input`, {
  # Make a sensor with custom characteristics
  sensor <- new("Sensor",
    Rmin = input$Rmin, Rmax = input$Rmax,
    delta = input$delta
  )

  # Create a specific sensor object
  sensor <- makeSpecificSensor(
    sensor, input$sensorType2,
    input$midpoint
  )

  # Create the appropriate bounds
  bounds <- boundsLookup[[input$sensorType2]]

  # Pass the sensor type
  sensor_type <- input$sensorType2
  sensorInfo$sensor_name <- input$sensorName2

  sensorInfo$sensor <- sensor
  sensorInfo$sensor_type <- sensor_type
  sensorInfo$bounds <- bounds
})

observeEvent(input$`use-upload`, {
  # Create a custom sensor
  inFile <- input$customSpectra

  # If there is an input file, we should parse it to create a sensor
  spectra <- read.csv(inFile$datapath, header = FALSE)
  spectra <- sensorOverlord::spectraMatrixFromValues(
    lambdas_minimum = spectra$V1,
    values_minimum = spectra$V2,
    lambdas_maximum = spectra$V3,
    values_maximum = spectra$V4
  )

  # Make easy-access variables for the wavelength range
  lambda1_low <- input$lambda1 - input$lambda1_size / 2
  lambda1_high <- input$lambda1 + input$lambda1_size / 2
  lambda2_low <- input$lambda2 - input$lambda2_size / 2
  lambda2_high <- input$lambda2 + input$lambda2_size / 2

  # Make a sensor from the spectra
  sensor <-
    newSensorFromSpectra(spectra,
      lambda_1 = c(lambda1_low, lambda1_high),
      lambda_2 = c(lambda2_low, lambda2_high)
    )

  # Create the appropiate sensor
  sensor_type <- input$sensorType
  sensor <- makeSpecificSensor(
    sensor, sensor_type,
    input$midpoint
  )

  # Create the appropriate bounds
  bounds <- boundsLookup[[sensor_type]]

  sensorInfo$sensor_name <- input$sensorName
  sensorInfo$sensor <- sensor
  sensorInfo$sensor_type <- sensor_type
})

getSensor <- reactive({

  # Make easy-access variables for the wavelength range
  lambda1_low <- input$lambda1 - input$lambda1_size / 2
  lambda1_high <- input$lambda1 + input$lambda1_size / 2
  lambda2_low <- input$lambda2 - input$lambda2_size / 2
  lambda2_high <- input$lambda2 + input$lambda2_size / 2

  # Create a sensor object from the input sensor, if it's
  # from the database
  if (input$sensors %in% sensorNames) {
    index <- match(input$sensors, sensorData$sensor_name)
    spectra <- spectraMatrixFromValues(
      lambdas_minimum = sensorData$lambda_min[[index]],
      values_minimum = sensorData$values_min[[index]],
      lambdas_maximum = sensorData$lambda_max[[index]],
      values_maximum = sensorData$values_max[[index]]
    )

    # Make a sensor from the spectra
    sensor <-
      newSensorFromSpectra(spectra,
        lambda_1 = c(lambda1_low, lambda1_high),
        lambda_2 = c(lambda2_low, lambda2_high)
      )

    # Create the appropiate sensor
    sensor_type <- sensorData$sensor_type[[index]]
    sensor <- makeSpecificSensor(
      sensor, sensor_type,
      sensorData$sensor_midpoint[[index]]
    )
    sensorInfo$sensor_name <- input$sensors

    # Create the appropriate bounds
    bounds <- boundsLookup[[sensor_type]]

    sensorInfo$sensor <- sensor
    sensorInfo$sensor_type <- sensor_type
    sensorInfo$bounds <- bounds
  }
})

get_accuracies <- reactive({
  sort(lapply(strsplit(input$acc, ","), as.numeric)[[1]])
})

get_ranges_df <- reactive({
  ranges_df(sensorInfo$sensor,
    inaccuracies = input$relErr,
    thresholds = get_accuracies(),
    by = boundsLookup[[sensorInfo$sensor_type]][3]
  )
})

get_error_df <- reactive({
  error_df(sensorInfo$sensor,
    inaccuracies = input$relErr,
    by = boundsLookup[[sensorInfo$sensor_type]][3]
  )
})
# Home Page -------------------------------------------------

# Output a dumbell plot of the ranges we can measure with
# this microscope precision and desired accuracy
output$range <- renderPlot({
  input$graphUpdate
  # Get the sensor
  isolate(getSensor())

  rangePlot(isolate(sensorInfo$sensor),
    ranges = isolate(get_ranges_df()),
    ylim = isolate(sensorInfo$bounds[1:2])
  ) +
    theme(
      axis.title = element_text(size = rel(1.5)),
      axis.text.y = element_text(size = rel(1.5)),
      aspect.ratio = 1 / 5
    ) +
    scale_x_discrete(
      labels = c(isolate(sensorInfo$sensor_name))
    )
})

# Make a phase plot for the sensor
output$phasePlot <- renderPlotly({
  input$graphUpdate
  error_table <- isolate(get_error_df())
  sensor_type <- colnames(error_table)[1]
  error_table <- error_table[, c(sensor_type, "Error")]
  error_table <- error_table[Reduce(`&`, lapply(error_table, is.finite)), ]
  plot_ly(
    data = error_table, x = error_table[, sensor_type], y = ~Error,
    hoverinfo = "text", text = paste0(
      sensor_type, ": ", round(error_table[, sensor_type], 2), "\n",
      "Error: ", round(error_table[, "Error"], 2)
    )
  ) %>%
    add_lines() %>%
    layout(
      yaxis = list(
        range = c(0, min(error_table$Error) * 5)
      ),
      xaxis = list(
        title = sensor_type
      )
    ) %>%
    config(displaylogo = FALSE)
})


# Outputs the characteristics of the current sensor as text
output$sensorChars <- renderText({
  sensor <- sensorInfo$sensor
  main <- paste(
    "Rmin: ", round(sensor@Rmin, 2),
    "| Rmax: ", round(sensor@Rmax, 2),
    "| delta: ", round(sensor@delta, 2)
  )

  sensor_type <- sensorInfo$sensor_type

  midpoint <- switch(sensor_type, "redox" = round(sensor@e0, 2),
    "pH" = round(sensor@pH, 2),
    "other" = "NA"
  )

  paste(main, "| Midpoint: ", midpoint)
})

# Custom Sensor Page ---------------------------------------------------
# Output the characteristics of the custom sensor
output$customChars <- output$customChars2 <- renderText({
  # Make a sensor with custom characteristics
  sensor <- sensorInfo$sensor

  # Create a specific sensor object
  sensor <- makeSpecificSensor(
    sensor, input$sensorType,
    input$midpoint
  )
  return(paste0(
      "Sensor: ", sensorInfo$sensor_name, " | ",
      "Rmin: ", round(sensor@Rmin, 2), " | ",
      "Rmax: ", round(sensor@Rmax, 2), " | ",
      "Dynamic range: ", round(sensor@Rmax / sensor@Rmin, 3)))
})

# Output the graph of R vs FractionMax of the custom sensor
output$plotFractionMax_custom <- output$plotFractionMax_custom2 <- renderPlot({
  # Make a sensor with custom characteristics
  sensor <- sensorInfo$sensor

  return(plotFractionMax(sensor) +
    theme(
      aspect.ratio = 1,
      text = element_text(size = 20)
    ))
})

# Output the graph of R vs Value for the custom sensor
output$plotValue_custom <- output$plotValue_custom2 <- renderPlot({
  # Make a sensor with custom characteristics
  sensor <- sensorInfo$sensor

  R_Value <- data.frame(R = getR(sensor), Value = getProperty(
    sensor,
    getR(sensor)
  ))


  ggplot(R_Value, aes(x = R, y = Value)) +
    geom_line() +
    theme(
      aspect.ratio = 1,
      text = element_text(size = 20)
    )
})

# Full Error Table Page ------------------------------------------------
output$fullTable <- renderDataTable({
  get_error_df()
}, )
