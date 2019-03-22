# sensorSpectra constructors -----

#' A function to make a sensorSpectra from a set of 4 vectors
#'
#' I'll typically collect spectra data in an excel file with 4 columns: the lambda and emission
#' values corresponding to the minimum and maximum states. This function takes data from those 4
#' columns, condenses them into a 3-column dataframe (with matching lambda values) and makes
#' a sensorSpectra object
#'
#' @param lambdas_minimum Lambdas of the values corresponding to the Rmin state
#' @param values_minimum Emission values corresponding to the Rmin state
#' @param lambdas_maximum Lambdas of the values corresponding to the Rmax state
#' @param values_maximum Emission values corresponding to the Rmax state

#' @return A sensorSpectra object
#'
#' @export
#' @rdname spectraMatrixFromValues-function
#' @import stats
spectraMatrixFromValues <- function(lambdas_minimum, values_minimum, lambdas_maximum, values_maximum) {
    # Trim any NA values
    lambdas_minimum <- na.omit(lambdas_minimum)
    values_minimum <- na.omit(values_minimum)
    lambdas_maximum <- na.omit(lambdas_maximum)
    values_maximum <- na.omit(values_maximum)

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
