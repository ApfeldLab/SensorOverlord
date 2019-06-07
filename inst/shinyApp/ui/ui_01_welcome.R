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
                     )
                 )
         )
         ),
mainPanel(
    fluidRow(
        box(
            h3("Range of suited values:"),
            plotlyOutput(outputId = "range")
        ),

        box(
            h3("Phase plot:"),
            plotlyOutput(outputId = "phasePlot")
        )
    )
)
)
)
