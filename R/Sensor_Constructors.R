# Sensor constructors ------

# lambda_1 and lambda_2 are arrays of length two describing the range of values to measure
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

# sensorSpectra constructors -----

# values_minimum represents state corresponding to Rmin
# values_maximum represents state corresponding to Rmax
# Turn two pairs of lambda-emission spectra into a 3-column dataframe
spectraMatrixFromValues <- function(lambdas_minimum, values_minimum, lambdas_maximum, values_maximum) {

    # Set beginning and ending lambda values
    # Warning: will trim lambdas not in the range of shared values
    start <- max(min(lambdas_minimum),
                 min(lambdas_maximum))
    end <- min(max(lambdas_minimum),
               max(lambdas_maximum))
    range <- seq(start, end, by = 0.1)

    new_values_minimum <- rescaleToRange(new_xs = range, old_xs = lambdas_minimum, y = values_minimum)
    new_values_maximum <- rescaleToRange(new_xs = range, old_xs = lambdas_maximum, y = values_maximum)

    return(new("sensorSpectra",
               lambdas = range, values_minimum = new_values_minimum, values_maximum = new_values_maximum))

}
