tabPanel("Settings",
      sidebarLayout(
                  sidebarPanel(width = 4,
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
                      ),

                      h3(textOutput(outputId = "numR")),
                      h3(textOutput(outputId = "precision"))
                  ),

                  mainPanel(
                      plotOutput(outputId = 'numRHist'),
                      plotOutput(outputId = 'precisionHist')
                  )
              )
)

