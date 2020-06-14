tabPanel("Main Analysis",
         value = "analysis",
         sidebarLayout(
             sidebarPanel(
                 width = 4,
                 h3("Sensor Parameters", align = "center"),
                 hr(),
                 fluidRow(
                     # Select a sensor
                     h4("What sensor would you like to use?", align = "left"),
                     selectizeInput(
                         inputId = "sensors",
                         label = "Select a sensor",
                         choices = list(
                             Custom = c("Custom"),
                             redox =  sensorNames_redox,
                             pH = sensorNames_pH),
                         selected = sensorNames_redox[1],
                         multiple = FALSE
                     ),
                     br(),
                     p("We have loaded a few pre-analysed sensors to get you started.
          Feel free to choose one of those from the dropdown box."),
                     p("Once you're
          comfortable, select 'Custom', and then scroll to the bottom of the page
                       to input the characteristics of your sensor of choice.")
                 ),
                 hr(),
                 fluidRow(
                     h4("What is your microscopy precision, and how accurate would you like to be?", align = "left"),
                     column(
                         width = 6,
                         numericInput(
                             inputId = "relErr",
                             label = "Relative Microscopy Imprecision",
                             min = 0,
                             max = Inf,
                             step = 0.01,
                             value = 0.01
                         )
                     ),
                     column(
                         width = 6,
                         textInput(
                             inputId = "acc",
                             label = "Accuracy (comma-seperated)",
                             value = "0.5,1.0,1.5"
                         )
                     ),
                     br(),
                     p("You'll need to determine your microscopy imprecision empirically. For a reference,
          we usually observe 2.8% error, but that can vary from 1% to 4% depending on image
          analysis methods and experimental conditions."),
                     p("Accuracy can be determined as you see fit!")
                 ),
                 hr(),
                 fluidRow(
                     h4("What wavelengths are you using to make your measurements?"),
                     column(
                         width = 6,
                         # Input the first wavelength
                         numericInput(
                             inputId = "lambda1",
                             label = "First Wavelength",
                             min = 0,
                             max = 1000,
                             step = 1,
                             value = 405
                         )
                     ),
                     column(
                         width = 6,
                         numericInput(
                             inputId = "lambda2",
                             label = "Second Wavelength",
                             min = 0,
                             max = 1000,
                             step = 1,
                             value = 470
                         )
                     )
                 ),
                 fluidRow(
                     column(
                         width = 6,
                         numericInput(
                             inputId = "lambda1_size",
                             label = "Band Size (1st Wavelength)",
                             min = 1,
                             max = 100,
                             step = 1,
                             value = 10
                         )
                     ),
                     column(
                         width = 6,
                         numericInput(
                             inputId = "lambda2_size",
                             label = "Band Size (2nd Wavelength)",
                             min = 1,
                             max = 100,
                             step = 1,
                             value = 10
                         )
                     ),
                     br(),
                     p("You may find that adjusting your wavelengths or narrowing your band
          size may affect the ultimate accuracy of your measurements.")
                 ),
                 hr(),
                 actionButton("graphUpdate", label = "Run/Update Analysis", width = "100%")
             ),
             mainPanel(
                 h3("Suitable Range", align = "center"),
                 plotOutput(outputId = "range") %>% withSpinner(type = 6),
                 h3("Phase Plot", align = "center"),
                 plotlyOutput(outputId = "phasePlot", height = "500%", width = "100%") %>% withSpinner(type = 6)
             )
         ),
         hr(),
         h1("Custom Sensor Input"),
         p("SensorOverlord needs Rmin, Rmax, delta, and midpoint values.
               It can either determine some of those from an uploaded spectra file ('Upload Spectra' tab),
               or you can provide those parameters ('Input Characteristics' tab)."),
         tabsetPanel(
             tabPanel("Upload Spectra",
                    sidebarLayout(
                        sidebarPanel(
                            # Upload a file
                            fileInput(
                                inputId = "customSpectra",
                                label = "Upload spectra as .csv"
                            ),

                            textInput(
                                inputId = "sensorName",
                                label = "Sensor Name",
                                value = "My Sensor"
                            ),
                            # Type of sensor
                            radioButtons(
                                inputId = "sensorType",
                                label = "Sensor Type",
                                choiceValues = c("redox", "pH", "other"),
                                choiceNames = c("Redox", "pH", "Other"),
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
                            ),

                            actionButton("use-upload", label = "Use This Sensor")
                        ),

                        mainPanel(
                            h3(textOutput("customChars")),
                            box(
                                plotOutput("plotFractionMax_custom")
                            ),
                            box(
                                plotOutput("plotValue_custom")
                            )

                        )
                    )
             ),
             # TODO: Modularize
             tabPanel("Input Characteristics",
                      sidebarLayout(
                          sidebarPanel(
                              textInput(
                                  inputId = "sensorName2",
                                  label = "Sensor Name",
                                  value = "My Sensor"
                              ),
                              # Type of sensor
                              radioButtons(
                                  inputId = "sensorType2",
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
                                  inputId = "midpoint2",
                                  label = "Midpoint Value (E0, pKa, etc)
                                \n Note: Ignored for 'other' sensor type",
                                  min = -Inf,
                                  max = Inf,
                                  step = 1,
                                  value = 1
                              ),

                              actionButton("use-custom-input", "Use This Sensor")
                          ),

                          mainPanel(
                              h3(textOutput("customChars2")),
                              box(
                                  plotOutput("plotFractionMax_custom2")
                              ),
                              box(
                                  plotOutput("plotValue_custom2")
                              )
                          )


                      )
             )
     )
)
