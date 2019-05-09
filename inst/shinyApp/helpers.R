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

