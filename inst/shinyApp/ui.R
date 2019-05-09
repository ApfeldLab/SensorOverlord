require(shiny)
require(shinydashboard)
require(sensorOverlord)
require(dplyr)
require(ggplot2)
require(ggalt)
require(mongolite)
source("helpers.R")

#' @import shinydashboard

# Define UI for application that draws a histogram
# Sidebar definition ------------------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview", tabName = "overview"),
        menuItem("Settings", tabName = "settings"))
)



# body --------------------------------------------------------------------
body <- dashboardBody(
    tabItems(
        # Overview Tab ------------------------------------------------------------
        tabItem(tabName = "overview",
                    # Welcome message
                    h1("Welcome to Sensor Overlord"),
                    p("Welcome to the app. Please explore! To get started, enter your
              sensor's characteristics below, along with your microscopy error
              model and desired accuracy, and I'll give you an estimate of the
              values that your sensor is well-suited to measure!"),
                    br(),




                    # Input boxes
                    fluidRow(

                        # Sensor characteristics box
                        box(
                            title = "Sensor Characteristics",

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

                            # E0 selection
                            conditionalPanel(
                                condition = "input.sensorType == 'redox'",
                                numericInput(
                                    inputId = "e0",
                                    label = "Midpoint Potential",
                                    min = -Inf,
                                    max = Inf,
                                    step = 1,
                                    value = -250

                                )
                            ),

                            # pKa selection
                            conditionalPanel(
                                condition = "input.sensorType == 'pH'",
                                numericInput(
                                    inputId = "pKa",
                                    label = "pKa",
                                    min = -Inf,
                                    max = Inf,
                                    step = 0.1,
                                    value = 7.0
                                )
                            )

                        ),
                        box(
                            title = "Microscopy Errors and Accuracy",

                            numericInput(
                                inputId = "relErr",
                                label = "Relative Error",
                                min = 0,
                                max = Inf,
                                step = 0.01,
                                value = 0.01
                            ),

                            numericInput(
                                inputId = "absErr",
                                label = "Absolute Error",
                                min = 0,
                                max = Inf,
                                step = 0.01,
                                value = 0
                            ),

                            numericInput(
                                inputId = "acc",
                                label = "Accuracy",
                                min = 0,
                                max = Inf,
                                step = 0.01,
                                value = 2
                            ),

                            selectInput(
                                inputId = "sensors",
                                label = "Sensors",
                                choices = c("None", sensorNames),
                                multiple = FALSE
                            ),

                            numericInput(
                                inputId = "rpres",
                                label = "General R precision (log10: smaller = more computation)",
                                min = -Inf,
                                max = Inf,
                                step = 0.01,
                                value = -2
                            ),

                            numericInput(
                                inputId = "rpres_edge",
                                label = "Fold-increase of precision
                        near edges (log10: larger = more computation)",
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
                            textOutput(outputId = "precision")
                        )
                    ),

                    fluidRow(
                        box(align="center",
                            width = 12,
                            plotOutput(outputId = "range")

                        )
                    )

            # Close overview tab
            ),


        # Settings Tab-----------------------------------------------------------
        tabItem(tabName = "settings",
                h1("Settings!"))

    # Close tabItems and dashboardBody ----------------------------------------
    )
)


# Main Page ---------------------------------------------------------------
dashboardPage(
    # Header
    dashboardHeader(title = "Sensor Overlord"),
    sidebar,
    body
)


