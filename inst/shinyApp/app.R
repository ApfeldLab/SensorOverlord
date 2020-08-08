library(shiny)
library(shinydashboard)
library(sensorOverlord)
library(dplyr)
library(ggplot2)
library(ggalt)
library(mongolite)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(DT)
library(gridExtra)
source(file.path("utils", "helpers.R"),  local = TRUE)

ui <- navbarPage(
    title = "S.O.",
    id="Sensoroverlord",
    fluid=TRUE,
    theme = "bootstrap.min.css",
    source(file.path("ui", "ui_01_welcome.R"), local = TRUE)$value,
    source(file.path("ui", "ui_02_analysis.R"), local = TRUE)$value,
    source(file.path("ui", "ui_03_more.R"), local = TRUE)$value,
    #source(file.path("ui", "ui_04_settings.R"), local = TRUE)$value,
    footer = includeHTML("www/footer.html")
)

server <- function(input, output, session) {
    source(file.path("server", "server_01_welcome.R"), local = TRUE)$value
    source(file.path("server", "server_02_custom.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
