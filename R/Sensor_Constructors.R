# Sensor constructors ------

# lambda_1 and lambda_2 are arrays of length two describing the range of values to measure
#' @export
newSensorFromSpectra <- function(sensorSpectra, lambda_1, lambda_2) {
    values_maximum = sensorSpectra@values_maximum
    values_minimum = sensorSpectra@values_minimum
    lambdas = sensorSpectra@lambdas

    # Make boolean vectors describing which of the lambdas are relevant
    relevant_lambda_1 <- (lambdas >= lambda_1[1] & lambdas <= lambda_1[2])
    relevant_lambda_2 <- (lambdas >= lambda_2[1] & lambdas <= lambda_2[2])

    # Set the parameters required for a sensor
    delta <- mean(values_maximum[relevant_lambda_2]) /
        mean(values_minimum[relevant_lambda_2])

    Rmin <- mean(values_minimum[relevant_lambda_1]) /
        mean(values_minimum[relevant_lambda_2])

    Rmax <- mean(values_maximum[relevant_lambda_1]) /
        mean(values_maximum[relevant_lambda_2])

    # Make a new sensor
    return(new("Sensor", Rmax = Rmax, Rmin = Rmin, delta = delta))
}
