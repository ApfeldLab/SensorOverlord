tabPanel(
  "Home",
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
  hr(),
  fluidRow(
      column(6,
             h2("Purpose"),
             includeHTML("www/purpose.html")
      ),

      column(6,
             h2("Instructions"),
             includeHTML("www/instructions.html")
      )
  ),
  hr(),
  h2("Ready? Get started here:"),
  actionButton("link_to_analysis", "Analysis Page")
)
