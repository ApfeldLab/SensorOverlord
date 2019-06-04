#' A wrapper function to run the included Shiny App
#' @export
#' @import shiny
launchApp <- function() {
    appDir <- system.file("shinyApp", package = "sensorOverlord")

    if (appDir == "") {
        stop("Could not find the Shiny App directory. Try reinstalling sensorOverlord",
             call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
