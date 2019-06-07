### Get data from database
options(mongodb = list(
    "host" = "sensoroverlordcluster-mnopd.mongodb.net",
    "username" = "sensoroverlord",
    "password" = "test"
))

databaseName <- "sensordb"
collectionName <- "responses"

getSensorData <- function() {
    # Connect to the database
    db <- mongo(collection = collectionName,
                url = sprintf(
                    "mongodb+srv://%s:%s@%s/%s",
                    options()$mongodb$username,
                    options()$mongodb$password,
                    options()$mongodb$host,
                    databaseName))

    # Read all the entries
    data_output <- unique(db$find())
    data_output
}

sensorData <- getSensorData()
sensorNames <- sensorData$'sensor_name'


# Make a specific type of sensor
makeSpecificSensor <- function(sensor, sensor_type, sensor_midpoint = 0) {
    if(sensor_type == "redox") {
        return(new("redoxSensor", sensor, e0 = sensor_midpoint))
    }

    if(sensor_type == "pH") {
        return(new("pHSensor", sensor, pKa = sensor_midpoint))
    }

    else {
        return(sensor)
    }
}



# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
    id <- button[["attribs"]][["id"]]
    div(
        `data-for-btn` = id,
        button,
        span(
            class = "btn-loading-container",
            hidden(
                img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
                icon("check", class = "btn-done-indicator")
            )
        ),
        hidden(
            div(class = "btn-err",
                div(icon("exclamation-circle"),
                    tags$b("Error: "),
                    span(class = "btn-err-msg")
                )
            )
        )
    )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
    # UX stuff: show the "busy" message, hide the other messages, disable the button
    loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
    doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
    errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
    shinyjs::disable(buttonId)
    shinyjs::show(selector = loadingEl)
    shinyjs::hide(selector = doneEl)
    shinyjs::hide(selector = errEl)
    on.exit({
        shinyjs::enable(buttonId)
        shinyjs::hide(selector = loadingEl)
    })

    # Try to run the code when the button is clicked and show an error message if
    # an error occurs or a success message if it completes
    tryCatch({
        value <- expr
        shinyjs::show(selector = doneEl)
        shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                           time = 10))
        value
    }, error = function(err){
        errorFunc(err, buttonId)
    })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
    errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
    errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
    errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
    shinyjs::html(html = errMessage, selector = errElMsg)
    shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

appCSS <- "
.btn-loading-container {
  margin-left: 10px;
  font-size: 1.2em;
}
.btn-done-indicator {
  color: green;
}
.btn-err {
  margin-top: 10px;
  color: red;
}
"
