
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
