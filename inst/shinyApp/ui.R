require(shiny)
require(shinydashboard)
require(sensorOverlord)
require(dplyr)
require(ggplot2)
require(ggalt)
require(mongolite)
source("helpers.R")

#' @import shinydashboard


welcomeMessage <- "
    Welcome to the SensorOverlord home page. Please select a sensor from
    our database, or go to 'Add a Custom Sensor' to input parameters of
    your own.
"

# Sidebar definition ------------------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Add a Custom Sensor", tabName = "newSensor"),
        menuItem("Full Error Table", tabName = "table"),
        menuItem("Settings", tabName = "settings"),
        menuItem("Browse Sensor Database", tabName = "browse"),
        menuItem("Contribute to Database", tabName = "upload"))
)



# Body definition --------------------------------------------------------------
body <- dashboardBody(
    tabItems(
        # Home Page ------------------------------------------------------------
        tabItem(tabName = "home",
                    # Welcome message
                    h3(welcomeMessage),
                    br(),

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
                    ),

                    # Plot the suitable ranges
                fluidRow(
                    box(
                        h3("Suitable range of selected sensor:",
                           align = "center"),
                        h3(textOutput("rangeText"),
                           align = "center")
                    ),

                    box(
                        h3("Sensor characteristics:",
                           align = "center"),
                        h3(textOutput("sensorChars"),
                           align = "center")
                    )
                ),

                    br(),

                fluidRow(
                    box(
                        plotOutput(outputId = "range")
                    ),

                    box(
                        plotOutput(outputId = "phasePlot")
                    )
                )

            # Close overview tab
            ),

        # Custom Sensor Page ---------------------------------------------------
        tabItem(tabName = "newSensor",
                # Sensor characteristics selection
                box(
                    title = "Input sensor characteristics",

                    # Upload a file
                    fileInput(
                        inputId = "customSpectra",
                        label = "Upload spectra as .csv"
                    ),

                    # Type of sensor
                    radioButtons(
                        inputId = "sensorType",
                        label = "Sensor Type",
                        choiceValues = c("redox", "pH", "other"),
                        choiceNames = c("Redox", "pH", "Other"),
                    ),

                    # Rmin input
                    numericInput(
                        inputId = "Rmin",
                        label = "Rmin",
                        min = 0,
                        max = Inf,
                        step = 0.01,
                        value = 1
                    ),

                    # Rmax input
                    numericInput(
                        inputId = "Rmax",
                        label = "Rmax",
                        min = 0,
                        max = Inf,
                        step = 0.01,
                        value = 5
                    ),

                    # Delta input
                    numericInput(
                        inputId = "delta",
                        label = withMathJax("$$ \\delta_{\\lambda 2} $$"),
                        min = 0,
                        max = Inf,
                        step = 0.01,
                        value = 0.2
                    ),

                    # Midpoint selection
                    numericInput(
                        inputId = "midpoint",
                        label = "Midpoint Value (E0, pKa, etc)
                                \n Note: Ignored for 'other' sensor type",
                        min = -Inf,
                        max = Inf,
                        step = 1,
                        value = 1
                    )
                ),

                # Sensor characteristics output
                box(
                    h3(textOutput("customChars")),
                    br(),
                    plotOutput("plotFractionMax_custom"),
                    plotOutput("plotValue_custom")
        )
        ),



        # Full Error Table Page-------------------------------------------------
        tabItem(tabName = "table",
                h1("Full error table for the selected sensor"),
                dataTableOutput('fullTable')
                ),

        # Settings Page --------------------------------------------------------
        tabItem(tabName = "settings",
                h1("Settings!"),

                # Input boxes
                fluidRow(
                    box(
                        title = "Generating ratiometric intensity values",

                        numericInput(
                            inputId = "rpres",
                            label = "Distance between R values (log scale)",
                            min = -Inf,
                            max = Inf,
                            step = 0.01,
                            value = -2
                        ),

                        numericInput(
                            inputId = "rpres_edge",
                            label = "Fold-increase in distance between R values
                            near Rmin and Rmax (log scale)",
                            min = -Inf,
                            max = Inf,
                            step = 0.01,
                            value = 2
                        )
                    )

                ),

                fluidRow(
                    box(align="center",
                        width = 12,
                        h3(textOutput(outputId = "numR")),
                        h3(textOutput(outputId = "precision"))
                    ),

                    box(align="left",
                        width = 6,
                        plotOutput(outputId = 'numRHist')),
                    box(align="right",
                        width = 6,
                        plotOutput(outputId = "precisionHist"))
                )



        )


    # Close tabItems and dashboardBody ----------------------------------------
    )
)


# Putting the page components together ----------------------------------------
dashboardPage(skin = "blue",
    # Header
    dashboardHeader(title = "Sensor Overlord"),
    sidebar,
    body,
    title = "Sensor Overlord"
)


