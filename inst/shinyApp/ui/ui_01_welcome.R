tabPanel("Home",
         useShinyjs(),
         tags$style(appCSS),
         tags$div(
             class = "jumbotron",
             tags$div(
                 class = "",
                 fluidRow(
                     column(10, h1("Sensor Overlord"))

                 ),
                 p("Can your sensor make accurate biochemical measurements?")
             )
         ),
sidebarLayout(
         sidebarPanel(width = 3,
             fluidRow(
                 box(
                     align = 'left', width = 12,
                     title = "Choose a sensor and error model",

                     # Select a sensor
                     selectInput(
                         inputId = "sensors",
                         label = "Select a sensor",
                         choices = c("Custom (see 'Add a Custom Sensor')", sensorNames),
                         selected = sensorNames[1],
                         multiple = FALSE
                     ),

                     # Input a relative error
                     numericInput(
                         inputId = "relErr",
                         label = "Relative Error",
                         min = 0,
                         max = Inf,
                         step = 0.01,
                         value = 0.01
                     ),

                     # Input an absolute error
                     numericInput(
                         inputId = "absErr",
                         label = "Absolute error",
                         min = 0,
                         max = Inf,
                         step = 0.01,
                         value = 0
                     ),

                     # Input a desired accuracy
                     numericInput(
                         inputId = "acc",
                         label = "Desired accuracy",
                         min = 0,
                         max = Inf,
                         step = 0.01,
                         value = 2
                     ),

                     # Input the first wavelength
                     numericInput(
                         inputId = "lambda1",
                         label = "First Wavelength",
                         min = 0,
                         max = 1000,
                         step = 1,
                         value = 405
                     ),

                     # Input the second wavelength
                     numericInput(
                         inputId = "lambda2",
                         label = "Second Wavelength",
                         min = 0,
                         max = 1000,
                         step = 1,
                         value = 470
                     ),

                     # Input the band size of the first wavelength
                     numericInput(
                         inputId = "lambda1_size",
                         label = "Band Size (1st Wavelength)",
                         min = 1,
                         max = 100,
                         step = 1,
                         value = 10
                     ),

                     # Input the band size of the second wavelength
                     numericInput(
                         inputId = "lambda2_size",
                         label = "Band Size (2nd Wavelength)",
                         min = 1,
                         max = 100,
                         step = 1,
                         value = 10
                     )
                 )
         )
         ),
mainPanel(
    fluidRow(
        box(height = 1000,
            h3("Range of suited values:"),
            textOutput(outputId = "rangeText"),
            plotlyOutput(outputId = "range", height = "500%", width = "100%")
        ),

        box(height = 1000,
            h3("Phase plot:"),
            plotlyOutput(outputId = "phasePlot", height = "500%", width = "100%")
        )
    )
)
)
)
