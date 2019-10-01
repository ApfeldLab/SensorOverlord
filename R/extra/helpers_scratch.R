E <- function(R, Rmin, Rmax, delta, e0, temp = 295.15) {
    return(e0 - (8.315 * temp) / (2 * 96.48104) *
        log( (delta * (Rmax - R)) / (R - Rmin))
    )
}

# Returns R, given params and E
R_of_E <- function(E, Rmin, Rmax, delta, e0, temp = 295.15) {
    return(

        (
            Rmin * exp(((e0 - E) * 2 * 96.48104)/(8.315*temp)) + delta*Rmax
        )
     /
        (
            exp(((e0 - E) * 2 * 96.48104)/(8.315*temp)) + delta
        )
    )
}

# error_R: a function that takes an R and returns the error
Error_E <- function(E, Rmin, Rmax, delta, e0, error_R, temp = 295.15) {
        R <- R_of_E(E, Rmin, Rmax, delta, e0, temp)
        larger_E <- suppressWarnings(E(R + error_R(R), Rmin, Rmax, delta, e0, temp))
        larger_E[is.nan(larger_E)] <- Inf
        smaller_E <- suppressWarnings(E(R - error_R(R), Rmin, Rmax, delta, e0, temp))
        smaller_E[is.nan(smaller_E)] <- Inf
        return(list(E = E,
                    Max_Error = pmax(abs(E - larger_E), abs(E - smaller_E))))
}



