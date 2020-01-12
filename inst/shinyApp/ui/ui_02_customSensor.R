tabPanel("Custom Sensor",
         tabsetPanel(
             tabPanel("Upload Spectra",
                    sidebarLayout(
                        sidebarPanel(
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
             tabPanel("Input Characteristics",
                      sidebarLayout(
                          sidebarPanel(
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
                              )
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
