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

"

# Sidebar definition ------------------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Settings", tabName = "settings"),
        menuItem("Add a Sensor", tabName = "newSensor"),
        menuItem("Browse Sensor Database", tabName = "browse"),
        menuItem("Contribute to Database", tabName = "upload"))
)



# Body definition --------------------------------------------------------------------
body <- dashboardBody(
    tabItems(
        # Overview Tab ------------------------------------------------------------
        tabItem(tabName = "home",
                    # Welcome message
                    h1("Welcome to Sensor Overlord"),
                    h3(welcomeMessage),
                    br(),




                    fluidRow(
                        box(align = 'center', width = 12,
                        selectInput(
                            inputId = "sensors",
                            label = "Select a sensor",
                            choices = c("Custom (see input tab)", sensorNames),
                            selected = sensorNames[1],
                            multiple = FALSE
                            )
                        )
                    ),

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

                            # # E0 selection
                            # conditionalPanel(
                            #     condition = "input.sensorType == 'redox'",
                            #     numericInput(
                            #         inputId = "e0",
                            #         label = "Midpoint Potential",
                            #         min = -Inf,
                            #         max = Inf,
                            #         step = 1,
                            #         value = -250
                            #
                            #     )
                            # ),
#
#                             # pKa selection
#                             conditionalPanel(
#                                 condition = "input.sensorType == 'pH'",
#                                 numericInput(
#                                     inputId = "pKa",
#                                     label = "pKa",
#                                     min = -Inf,
#                                     max = Inf,
#                                     step = 0.1,
#                                     value = 7.0
#                                 )
#                             )

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
                            )


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


# Main Page ---------------------------------------------------------------
dashboardPage(skin = "black",
    # Header
    dashboardHeader(title = "Sensor Overlord"),
    sidebar,
    body,
    title = "Sensor Overlord"
)


