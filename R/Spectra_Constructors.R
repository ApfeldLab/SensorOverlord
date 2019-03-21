# sensorSpectra constructors -----

# values_minimum represents state corresponding to Rmin
# values_maximum represents state corresponding to Rmax
# Turn two pairs of lambda-emission spectra into a 3-column dataframe
#' @export
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
