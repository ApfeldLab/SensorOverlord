#' A function to rescale a certain (x,y) matrix pair into a new (x_new, y) pair.
#'
#' @param new_xs Numeric vector. The new x labels that we want to rescale to
#' @param old_xs Numeric vector. The x labels of the previous matrix
#' @param y Numeric vector. The original y values corresponding to old_xs
#'
#' @return Numeric vector corresponding to the y's rescaled to fit the x_new vector
#'
#' @export
#' @rdname reScaleToRange-function
#' @import stats
rescaleToRange <- function(new_xs, old_xs, y) {
    # Initalize rescaled y values
    new_y <- c()

    # Loop through each new x value
    for (new_x in new_xs) {
        # Initalize the closest old_x value found
        closest_x_difference <- Inf

        # Initalize the index corresponding to the closest old_x value found
        closest_x_index <- NaN

        # Loop through each index of old x
        for (old_x_index in 1:length(old_xs)) {
            new_old_diff <- abs(old_xs[old_x_index] - new_x)
            if (new_old_diff < closest_x_difference) {
                closest_x_difference <- new_old_diff
                closest_x_index <- old_x_index
            }
        }

        # Accumulate new y values corresponding to the closest old_x found
        new_y <- c(new_y, y[closest_x_index])
    }

    return(new_y)
}


#' Formats sensor information into a dataframe suitable for input into database
#'
#' @param name Character string. The name of the sensor
#' @param type The type of sensor. One of:
#' {redox, pH, ATP}
#' @param readout The readout of the sensor. One of:
#' {exitation ratiometric, emission ratiometric}
#' @param lambda_max The lambda values corresponding to the emission in the
#' maximum state (corresponding to Rmax)
#' @param values_max The emission values in the maximum state (corresponding
#' to Rmax)
#' @param lambda_min The lambda values corresponding to the emission in the
#' minimum state (corresponding to Rmin)
#' @param values_min The emission values in the minimum state (corresponding
#' to Rmin)
#' @param sensor_midpoint Numeric. The midpoint of the sensor. Depending
#' on your sensor, this could be:
#' {e0, pKa, log-midpoint}
#'
#' @return A list object suitable to be pushed to the mongo database
#'
#' @export
formatSpectraData <- function(name, type, readout, lambda_max, values_max,
                              lambda_min, values_min, sensor_midpoint) {

    # Data validation -------

    # Validating lengths
    if(length(name) != 1) {
        stop("Length of the name argument is not 1")
    }

    if(length(type) != 1) {
        stop("Length of the type argument is not 1")
    }

    if(length(readout) != 1) {
        stop("Length of the readout argument is not 1")
    }

    if(length(sensor_midpoint) != 1) {
        stop("Length of the sensor_midpoint argument is not 1")
    }


    if(length(lambda_max) != length(values_max)) {
        stop("The lambda and values for the maximum state have
             different lengths")
    }

    if(length(lambda_min) != length(values_min)) {
        stop("The lambda and values for the maximum state have
             different lengths")
    }

    # Validating types
    supported_types <- c("redox", "pH", "ATP")
    supported_readouts <- c("excitation ratiometric", "emission ratiometric")

    if(typeof(name) != "character") {
        stop("The name argument must be a character type")
    }

    if(typeof(type) != "character") {
        stop("The type argument must be a character type")
    }

    if(typeof(readout) != "character") {
        stop("The readout argument must be a character type")
    }

    if(!is.numeric(lambda_max)) {
        stop("lambda_max must be numeric")
    }

    if(!is.numeric(values_max)) {
        stop("values_max must be numeric")
    }

    if(!is.numeric(lambda_min)) {
        stop("lambda_min must be numeric")
    }

    if(!is.numeric(values_min)) {
        stop("values_min must be numeric")
    }

    if(!is.numeric(sensor_midpoint)) {
        stop("sensor_midpoint must be numeric")
    }

    # ---------

    # Create the final list
    return(list(sensor_name = name, sensor_type = type,
                sensor_readout = readout,
                lambda_max = lambda_max, values_max = values_max,
                lambda_min = lambda_min, values_min = values_min,
                sensor_midpoint = sensor_midpoint))
}

#' Returns the sensor database
#'
#' @return The sensor database
#'
#' @export
#' @import mongolite
getDb <- function() {
    options(mongodb = list(
        "host" = "sensoroverlordcluster-mnopd.mongodb.net",
        "username" = "sensoroverlord",
        "password" = "test"
    ))

    databaseName <- "sensordb"
    collectionName <- "responses"

    return(
        mongo(collection = collectionName,
              url = sprintf(
                  "mongodb+srv://%s:%s@%s/%s",
                  options()$mongodb$username,
                  options()$mongodb$password,
                  options()$mongodb$host,
                  databaseName))
    )
}

#' Adjusts a spectra, assuming that the actual spectra is not limiting
#'
#' @param spectra a sensorSpectra object containing the spectra to be adjusted
#' @param fractionMax numeric [0->1] describing the preportion of limiting of
#' the curve corresponding to the Rmax state
#' @param fractionMin numeric [0->1] describing the preportion of limiting of
#' the curve corresponding to the Rmin state
#'
#' @return a sensorSpectra object containing the adjusted spectra
#'
#' @export
#' @import mongolite
adjustSpectra <- function(spectra, fractionMax, fractionMin) {
    adjusted_minimum <- (
                        (spectra@values_minimum / (1-fractionMin)) -
                        (spectra@values_maximum / fractionMax)
                        ) /
                        (
                        (fractionMin/(1-fractionMin)) -
                        ((1-fractionMax)/fractionMax)
                        )

    adjusted_maximum <- (
                        (spectra@values_maximum / (1-fractionMax)) -
                        (spectra@values_minimum / fractionMin)
                        ) /
                        (
                        (fractionMax/(1-fractionMax)) -
                        ((1-fractionMin)/fractionMin)
                        )

    return(new("sensorSpectra", lambdas = spectra@lambdas,
               values_minimum = adjusted_minimum,
               values_maximum = adjusted_maximum))


}





