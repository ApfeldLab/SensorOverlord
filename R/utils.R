# Rescale y to the same size as the new x values
#' @export
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
