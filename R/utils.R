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
formatSpectraData <-
  function(name,
           type,
           readout,
           lambda_max,
           values_max,
           lambda_min,
           values_min,
           sensor_midpoint) {
    # Data validation -------

    # Validating lengths
    if (length(name) != 1) {
      stop("Length of the name argument is not 1")
    }

    if (length(type) != 1) {
      stop("Length of the type argument is not 1")
    }

    if (length(readout) != 1) {
      stop("Length of the readout argument is not 1")
    }

    if (length(sensor_midpoint) != 1) {
      stop("Length of the sensor_midpoint argument is not 1")
    }


    if (length(lambda_max) != length(values_max)) {
      stop("The lambda and values for the maximum state have
             different lengths")
    }

    if (length(lambda_min) != length(values_min)) {
      stop("The lambda and values for the minimum state have
             different lengths")
    }

    # Validating types
    supported_types <- c("redox", "pH", "ATP")
    supported_readouts <-
      c("excitation ratiometric", "emission ratiometric")

    if (typeof(name) != "character") {
      stop("The name argument must be a character type")
    }

    if (typeof(type) != "character") {
      stop("The type argument must be a character type")
    }

    if (typeof(readout) != "character") {
      stop("The readout argument must be a character type")
    }

    if (!is.numeric(lambda_max)) {
      stop("lambda_max must be numeric")
    }

    if (!is.numeric(values_max)) {
      stop("values_max must be numeric")
    }

    if (!is.numeric(lambda_min)) {
      stop("lambda_min must be numeric")
    }

    if (!is.numeric(values_min)) {
      stop("values_min must be numeric")
    }

    if (!is.numeric(sensor_midpoint)) {
      stop("sensor_midpoint must be numeric")
    }

    # ---------

    # Create the final list
    return(
      list(
        sensor_name = name,
        sensor_type = type,
        sensor_readout = readout,
        lambda_max = lambda_max,
        values_max = values_max,
        lambda_min = lambda_min,
        values_min = values_min,
        sensor_midpoint = sensor_midpoint
      )
    )
  }

#' Returns the sensor database
#'
#' @return The sensor database
#'
#' @export
#' @import mongolite
getDb <- function() {
  options(
    mongodb = list(
      "host" = "sensoroverlordcluster-mnopd.mongodb.net",
      "username" = "sensoroverlord",
      "password" = "test"
    )
  )

  databaseName <- "sensordb"
  collectionName <- "responses"

  return(mongo(
    collection = collectionName,
    url = sprintf(
      "mongodb+srv://%s:%s@%s/%s",
      options()$mongodb$username,
      options()$mongodb$password,
      options()$mongodb$host,
      databaseName
    )
  ))
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
  adjusted_minimum <- ((spectra@values_maximum / fractionMax) -
    (spectra@values_minimum / fractionMin)) /
    (((1 - fractionMax) / fractionMax) -
      ((1 - fractionMin) / fractionMin))

  adjusted_maximum <- ((spectra@values_maximum / (1 - fractionMax)) -
    (spectra@values_minimum / (1 - fractionMin))) /
    ((fractionMax / (1 - fractionMax)) -
      ((fractionMin / (1 - fractionMin))))

  return(
    new(
      "sensorSpectra",
      lambdas = spectra@lambdas,
      values_minimum = adjusted_minimum,
      values_maximum = adjusted_maximum
    )
  )
}

#' What is the fraction deprotenated of a certain pH, given the pKa?
#' Note: this generalizes to any ligand-binding sensor type, given a pKa/pKd
#' and a pLigand
#'
#' @param pH The pH
#' @param pKa The pKa
#'
#' @export
fraction_deprot <- function(pH, pKa) {
  return((10^(pH - pKa)) /
    (1 + 10^(pH - pKa)))
}


## Biophysical parameters ------------------------------------------------------

#' What is the redox potential (mV), given R/Rmin/Rmax/delta/midpoint/temperature?
#'
#' @param R the ratiometric fluorescence
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param e0 the sensor's midpoint potential
#' @param temp the temperature at which measurements were made
#' @return The redox potential (mV)
#' @examples
#' E(R = 3, Rmin = 1, Rmax = 5, delta = 0.2, e0 = -275)
#' @export
E <- function(R, Rmin, Rmax, delta, e0, temp = 295.15) {
  return(e0 - (8.315 * temp) / (2 * 96.48104) *
    log((delta * (Rmax - R)) / (R - Rmin)))
}

#' Finds pH, given R, Rmin, and Rmax
#' @param R the ratiometric fluorescence
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param pKa the sensor's pKa
#' @param ... unused
#' @returns the pH, numeric
#' @examples
#' pH(R = 2, Rmin = 1, Rmax = 5, delta = 0.2, pKa = 7)
#' @export
pH <- function(R, Rmin, Rmax, delta, pKa, ...) {
  return(pKa + log10(delta)
         + log10((Rmax - R) / (R - Rmin)))
}

#' Finds pLigand, given R, Rmin, and Rmax
#' @param R the ratiometric fluorescence
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param pKd the sensor's pKd
#' @param ... not used
#' @returns the pLigand, numeric
#' @examples
#' pLigand(R = 2, Rmin = 1, Rmax = 5, delta = 0.2, pKd = 7)
#' @export
pLigand <- function(R, Rmin, Rmax, delta, pKd, ...) {
  # Same as pH
  return(pH(R = R, Rmin = Rmin, Rmax = Rmax, delta = delta, pKa = pKd))
}

# Inverse biophysical parameters -----------------------------------------------

#' What was the recorded fluorescence ratio at a certain redox potential?
#'
#' @param E the redox potential
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param e0 the sensor's midpoint potential
#' @param temp the temperature at which measurements were made
#' @return the ratiometric fluorescence associated with the given parameters
#' @examples
#' R_of_E(E = -254.5305, Rmin = 1, Rmax = 5, delta = 0.2, e0 = -275)
#'
#' # This is self-consistent with the E() function.
#' R_of_E(
#'   E = E(R = 3, Rmin = 1, Rmax = 5, delta = 0.2, e0 = -275),
#'   Rmin = 1, Rmax = 5, delta = 0.2, e0 = -275
#' )
#' @export
R_of_E <- function(E, Rmin, Rmax, delta, e0, temp = 295.15) {
  return(
    (
      Rmin * exp(((e0 - E) * 2 * 96.48104) / (8.315 * temp)) + delta * Rmax
    )
    /
      (
        exp(((e0 - E) * 2 * 96.48104) / (8.315 * temp)) + delta
      )
  )
}

#' Finds R, given a pH
#' @param pH the sensor's pH
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param pKa the sensor's pKa
#' @param ...  Not used
#' @returns the ratiometric fluoresence at this pH
#' @examples
#' R_of_pH(pH = 3, Rmin = 1, Rmax = 5, delta = 0.2, pKa = 7)
#' @export
R_of_pH <- function(pH, Rmin, Rmax, delta, pKa, ...) {
  A <- 10^(pH - pKa) / delta
  (Rmax + A*Rmin) / (A + 1)
}

#' Finds R, given a pLigand
#' @param pLigand the sensor's pLigand
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param pKd the sensor's pKd
#' @returns the ratiometric fluoresence at this pH
#' @examples
#' R_of_pLigand(pLigand = 2, Rmin = 1, Rmax = 5, delta = 0.2, pKd = 7)
#' @export
R_of_pLigand <- function(pLigand, Rmin, Rmax, delta, pKd, ...) {
  R_of_pH(pH = pLigand, Rmin = Rmin, Rmax = Rmax, delta = delta,
          pKa = pKd)
}

# Errors (one output)-----------------------------------------------------------

#' A general function for calculating the error in some parameter
#'
#' @param param The parameter over which we are calculating error
#' @param param_name The name of the parameter over which we are calculating error
#' @param R_of_param A function that maps from param --> R
#' NOTE: R_of_param must have arguments in the correct ordrer. They should be:
#' \itemize{
#' \item{"param"}
#' \item{"Rmin"}
#' \item{"Rmax"}
#' \item{"delta"}
#' \item{"midpoint"}
#' \item{"temperature, in kelvin"}
#' }
#' @param param_of_R A function that maps from R --> param
#' NOTE: param_of_R must have arguments in the correct order. They should be:
#' #' \itemize{
#' \item{"R"}
#' \item{"Rmin"}
#' \item{"Rmax"}
#' \item{"delta"}
#' \item{"midpoint"}
#' \item{"temperature, in kelvin"}
#' }
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param midpoint the sensor's midpoint (e.g. e0 for redox, pKa for pH, pKd for other ligand)
#' @param error_R a function that, given an R, returns the error in that R
#' @param temp the temperature at which measurements were made
#' @return A list with (1) 'E', the given redox potential,
#' (2) 'larger_E', the largest possible E, at the given error
#' (3) 'smaller_E', the smallest possible E, at the given error
#' (4) 'max_error' the largest possible difference between the observed and expected E
#'
#' @examples
#' Error_general(param = -275, param_name = "E", R_of_param = R_of_E, param_of_R = sensorOverlord::E,
#' Rmin = 1, Rmax = 5, delta = 0.2, midpoint = -275, error_R = function(x) 0.02 * x)
#' @export
Error_general <- function(param, param_name, R_of_param, param_of_R,
                          Rmin, Rmax, delta, midpoint, error_R, temp = 295.15) {
  R <- R_of_param(param, Rmin, Rmax, delta, midpoint, temp)
  larger_param <- suppressWarnings(param_of_R(R + error_R(R),
                                              Rmin, Rmax, delta, midpoint, temp))
  larger_param[is.nan(larger_param)] <- Inf
  smaller_param <- suppressWarnings(param_of_R(R - error_R(R),
                                               Rmin, Rmax, delta, midpoint, temp))
  smaller_param[is.nan(smaller_param)] <- Inf
  return_list <- list()
  return_list[[param_name]] = param
  return_list[[paste0("larger_", param_name)]] = larger_param
  return_list[[paste0("smaller_", param_name)]] = smaller_param
  return_list$max_error = pmax(abs(param - larger_param), abs(param - smaller_param))
  return_list
}

#' What is the error in redox potential at a given redox potential (mV),
#' given some parameters of R and the error in R?
#'
#' @param E the redox potential
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param e0 the sensor's midpoint potential
#' @param error_R a function that, given an R, returns the error in that R
#' @param temp the temperature at which measurements were made
#' @return A list with (1) 'E', the given redox potential,
#' (2) 'larger_E', the largest possible E, at the given error
#' (3) 'smaller_E', the smallest possible E, at the given error
#' (4) 'max_error' the largest possible difference between the observed and expected E
#'
#' @examples
#' Error_E(E = -275, Rmin = 1, Rmax = 5, delta = 0.2, e0 = -275, error_R = function(x) 0.02 * x)
#' @export
Error_E <- function(E, Rmin, Rmax, delta, e0, error_R, temp = 295.15) {
  Error_general(param = E, param_name = "E",
                R_of_param = R_of_E, param_of_R = sensorOverlord::E,
                Rmin = Rmin, Rmax = Rmax, delta = delta,
                midpoint = e0, error_R = error_R, temp = temp)
}

#' What is the error in pH at a given pH,
#' given some parameters of R and the error in R?
#'
#' @param pH the pH
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param pKa the sensor's midpoint/pKa
#' @param error_R a function that, given an R, returns the error in that R
#' @param ... not used (for generalization compatability)
#' @return A list with (1) 'pH', the given pH,
#' (2) 'larger_pH', the largest possible E, at the given error
#' (3) 'smaller_pH', the smallest possible E, at the given error
#' (4) 'max_error' the largest possible difference between the observed and expected E
#'
#' @examples
#' Error_pH(pH = 7.3, Rmin = 1, Rmax = 5, delta = 0.2, pKa = 8, error_R = function(x) 0.02 * x)
#' @export
Error_pH <- function(pH, Rmin, Rmax, delta, pKa, error_R, ...) {
  Error_general(param = pH, param_name = "pH",
                R_of_param = R_of_pH, param_of_R = sensorOverlord::pH,
                Rmin = Rmin, Rmax = Rmax, delta = delta,
                midpoint = pKa, error_R = error_R)
}

#' What is the error in pLigand at a given pLigand,
#' given some parameters of R and the error in R?
#'
#' @param pLigand the pLigand
#' @param ligand_name the name of the ligand
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param pKd the sensor's midpoint/pKd
#' @param error_R a function that, given an R, returns the error in that R
#' @param ... not used (for generalization compatability)
#' @return A list with (1) 'pH', the given pH,
#' (2) 'larger_pH', the largest possible E, at the given error
#' (3) 'smaller_pH', the smallest possible E, at the given error
#' (4) 'max_error' the largest possible difference between the observed and expected E
#'
#' @examples
#' Error_pLigand(pLigand = 7.3, ligand_name = "pNADPH",
#' Rmin = 1, Rmax = 5, delta = 0.2, pKd = 8, error_R = function(x) 0.02 * x)
#' @export
Error_pLigand <- function(pLigand, Rmin, Rmax, delta, pKd, error_R, ligand_name, ...) {
  Error_general(param = pLigand, param_name = ligand_name,
                R_of_param = R_of_pLigand, param_of_R = sensorOverlord::pLigand,
                Rmin = Rmin, Rmax = Rmax, delta = delta,
                midpoint = pKd, error_R = error_R)
}

# Error dataframes--------------------------------------------------------------

#' Creates a dataframe of errors at given inaccuracies
#'
#' @param inaccuracies A vector of inaccuracies (e.g. 0.02 for 2\% error), always relative
#' @param param_name The name of the parameter
#' @param param_error_fun The function that finds the error in this parameter
#' @param param_min The minimum param for which to record error
#' @param param_max The maximum param in mV, for which to record error
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param midpoint the sensor's midpoint
#' @param temp (optional, default: 295.15) the temperature (in Kelvin) at which measurements were made
#' @param by (optional, default: 0.01) The granularity of the error table--e.g., by = 0.01 would record 275 and 275.01, etc.
#' @param ... Additional arguments to pass into the param_error_fun (before temperature)
#' @return A dataframe of errors with columns:
#' param: the value of the param
#' 'Error': the error in this param
#' 'Inaccuracy': The inaccuracy of the measurements (relative to R).
#' @examples
#' create_error_df_general(c(0.01, 0.02), "E", Error_E, -300, -200, 1, 5, 0.2, -275)
#' @export
create_error_df_general <- function(inaccuracies, param_name,
                                    param_error_fun,
                                    param_min, param_max, Rmin, Rmax, delta, midpoint, ..., temp = 295.15, by = 0.01) {
  error_df_full <- data.frame()
  error_df_full[[param_name]] = c()
  error_df_full$Error = c()
  error_df_full$Inaccuracy = c()

  for (inaccuracy in inaccuracies) {
    error <- param_error_fun(
      seq(param_min, param_max, by = by),
      Rmin, Rmax, delta, midpoint,
      error_R = function(x) inaccuracy * x, ...,
      temp = temp
    )
    new_df <- list()
    new_df[[param_name]] = error[[param_name]]
    new_df$Error = error$max_error
    new_df$Inaccuracy = as.character(rep(inaccuracy, length(error$max_error)))

    error_df_full <- rbind(
      error_df_full,
      data.frame(new_df, stringsAsFactors = FALSE)
    )
  }
  error_df_full
}

#' Creates a dataframe of errors in redox potential at given inaccuracies
#'
#' @param inaccuracies A vector of inaccuracies (e.g. 0.02 for 2\% error), always relative
#' @param Emin The minimum redox potential, in mV, for which to record error
#' @param Emax The maximum redox potential, in mV, for which to record error
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param e0 the sensor's midpoint potential
#' @param temp (optional, default: 295.15) the temperature (in Kelvin) at which measurements were made
#' @param by (optional, default: 0.01) The granularity of the error table--e.g., by = 0.01 would record 275 and 275.01, etc.
#' @return A dataframe of errors with columns:
#' 'E': the redox potential (mV),
#' 'Error': the error in this redox potential (mV)
#' 'Inaccuracy': The inaccuracy of the measurements (relative to R).
#' @examples
#' create_error_df_redox(c(0.01, 0.02), -300, -200, 1, 5, 0.2, -275)
#' @export
create_error_df_redox <- function(inaccuracies, Emin, Emax, Rmin, Rmax, delta, e0, temp = 295.15, by = 0.01) {
  create_error_df_general(inaccuracies = inaccuracies, param_name = "E",
                          param_error_fun = Error_E, param_min = Emin, param_max = Emax,
                          Rmin = Rmin, Rmax = Rmax, delta = delta,
                          midpoint = e0, temp = temp, by = by)
}

#' Creates a dataframe of errors in pH potential at given inaccuracies
#'
#' @param inaccuracies A vector of inaccuracies (e.g. 0.02 for 2\% error), always relative
#' @param pHmin The minimum pH for which to record error
#' @param pHmax The maximum pH for which to record error
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param pKa the sensor's midpoint/pKa
#' @param temp (optional, default: 295.15) the temperature (in Kelvin) at which measurements were made
#' @param by (optional, default: 0.01) The granularity of the error table--e.g., by = 0.01 would record 275 and 275.01, etc.
#' @return A dataframe of errors with columns:
#' 'pH': the pH,
#' 'Error': the error in this pH
#' 'Inaccuracy': The inaccuracy of the measurements (relative to R).
#' @examples
#' create_error_df_pH(c(0.01, 0.02), 2, 10, 1, 5, 0.2, 7)
#' @export
create_error_df_pH <- function(inaccuracies, pHmin, pHmax, Rmin, Rmax, delta, pKa, by = 0.01) {
  create_error_df_general(inaccuracies = inaccuracies, param_name = "pH",
                          param_error_fun = Error_pH, param_min = pHmin, param_max = pHmax,
                          Rmin = Rmin, Rmax = Rmax, delta = delta,
                          midpoint = pKa, by = by)
}

#' Creates a dataframe of errors in pLigand potential at given inaccuracies
#'
#' @param inaccuracies A vector of inaccuracies (e.g. 0.02 for 2\% error), always relative
#' @param pLigand_min The minimum pLigand for which to record error
#' @param pLigand_max The maximum pLigand for which to record error
#' @param Rmin the minimum possible ratiometric fluorescence
#' @param Rmax the maximum possible ratiometric fluorescence
#' @param delta the ratiometric fluorescence in the first wavelength
#' @param pKd the sensor's midpoint/pKd
#' @param by (optional, default: 0.01) The granularity of the error table--e.g., by = 0.01 would record 275 and 275.01, etc.
#' @param ligand_name The name of this ligand
#' @return A dataframe of errors with columns:
#' 'pLigand': the pLigand,
#' 'Error': the error in this pLigand
#' 'Inaccuracy': The inaccuracy of the measurements (relative to R).
#' @examples
#' create_error_df_pLigand(c(0.01, 0.02), 2, 10, 1, 5, 0.2, 7,
#' ligand_name = "NADPH")
#' @export
create_error_df_pLigand <- function(inaccuracies, pLigand_min, pLigand_max, Rmin, Rmax, delta, pKd, by = 0.01, ligand_name = "ligand") {
  create_error_df_general(inaccuracies = inaccuracies, param_name = ligand_name,
                          param_error_fun = Error_pLigand, param_min = pLigand_min, param_max = pLigand_max,
                          Rmin = Rmin, Rmax = Rmax, delta = delta,
                          midpoint = pKd, by = by, ligand_name = ligand_name)
}

# Error dataframes at multiple inaccuracies ------------------------------------

#' Creates an error df at multiple inaccuracies, with multiple Rmin/Rmax/delta/e0 parameters
#' @param inaccuracies A vector of inaccuracies (e.g. 0.02 for 2\% error), always relative
#' @param Emin The minimum redox potential, in mV, for which to record error
#' @param Emax The maximum redox potential, in mV, for which to record error
#' @param param_df A dataframe containing a list of sensor parameters, with these columns:
#' 'name': An identifier for this sensor
#' 'Rmin': the minimum possible ratiometric fluorescence for this sensor
#' 'Rmax': the maximum possible ratiometric fluorescence for this sensor
#' 'delta': the ratiometric fluorescence in the first wavelength for this sensor
#' 'e0': this sensor's midpoint potential
#' @param temp (optional, default: 295.15) the temperature (in Kelvin) at which measurements were made
#' @param by (optional, default: 0.01) The granularity of the error table--e.g., by = 0.01 would record 275 and 275.01, etc.
#' @return A dataframe of errors with columns:
#' 'Name': this sensor name
#' 'E': the redox potential (mV),
#' 'Rmin': the minimum possible ratiometric fluorescence for this sensor
#' 'Rmax': the maximum possible ratiometric fluorescence for this sensor
#' 'Error': the error in this redox potential (mV)
#' 'Inaccuracy': The inaccuracy of the measurements (relative to R).
#' @examples
#' create_error_df_redox_multiple(
#' c(0.01, 0.02), -300, -200,
#' data.frame(
#'     "Rmin" = c(1, 2),
#'     "Rmax" = c(5, 6),
#'     "delta" = c(0.2, 1.2),
#'     "name" = c("normal", "plusOne"),
#'     "e0" = c(-275, -274)
#' )
#' )
#' @export
create_error_df_redox_multiple <- function(inaccuracies, Emin, Emax, param_df, temp = 295.15, by = 0.01) {
  error_df_full <- data.frame(E = c(), Rmin = c(), Rmax = c(),
                              Name = c(), Error = c(), Inaccuracy = c())
  # Loop through each sensor in the param_df
  for(n in 1:nrow(param_df)) {
    sensor_params <- param_df[n, ]

    new_error <- create_error_df_redox(inaccuracies, Emin = Emin, Emax = Emax,
                          Rmin = sensor_params[,"Rmin"], Rmax = sensor_params[,"Rmax"],
                          delta = sensor_params[,"delta"], e0 = sensor_params[,"e0"],
                          temp = temp, by = by)

    new_error$Rmin = as.character(rep(sensor_params[,"Rmin"], length(new_error$E)))
    new_error$Rmax = as.character(rep(sensor_params[,"Rmax"], length(new_error$E)))
    new_error$Name = as.character(rep(sensor_params[,"name"], length(new_error$E)))
    new_error %>%  mutate_if(is.factor, as.character) -> new_error

    error_df_full <- rbind(
      error_df_full,
      new_error
    )

  }
  error_df_full
}

#' Creates an error df at multiple inaccuracies, with multiple Rmin/Rmax/delta/pKa parameters
#' @param inaccuracies A vector of inaccuracies (e.g. 0.02 for 2\% error), always relative
#' @param pHmin The minimum pH for which to record error
#' @param pHmax The maximum pH for which to record error
#' @param param_df A dataframe containing a list of sensor parameters, with these columns:
#' 'name': An identifier for this sensor
#' 'Rmin': the minimum possible ratiometric fluorescence for this sensor
#' 'Rmax': the maximum possible ratiometric fluorescence for this sensor
#' 'delta': the ratiometric fluorescence in the first wavelength for this sensor
#' 'pKa': this sensor's midpoint/pKa
#' @param by (optional, default: 0.01) The granularity of the error table--e.g., by = 0.01 would record 275 and 275.01, etc.
#' @return A dataframe of errors with columns:
#' 'Name': this sensor name
#' 'pH': the pH
#' 'Rmin': the minimum possible ratiometric fluorescence for this sensor
#' 'Rmax': the maximum possible ratiometric fluorescence for this sensor
#' 'Error': the error in this pH
#' 'Inaccuracy': The inaccuracy of the measurements (relative to R).
#' @examples
#' create_error_df_pH_multiple(
#' c(0.01, 0.02), 2, 10,
#' data.frame(
#'     "Rmin" = c(1, 2),
#'     "Rmax" = c(5, 6),
#'     "delta" = c(0.2, 1.2),
#'     "name" = c("normal", "plusOne"),
#'     "pKa" = c(7, 8)
#' )
#' )
#' @export
create_error_df_pH_multiple <- function(inaccuracies, pHmin, pHmax, param_df, by = 0.01) {
  error_df_full <- data.frame(pH = c(), Rmin = c(), Rmax = c(),
                              Name = c(), Error = c(), Inaccuracy = c())
  # Loop through each sensor in the param_df
  for(n in 1:nrow(param_df)) {
    sensor_params <- param_df[n, ]

    new_error <- create_error_df_pH(inaccuracies, pHmin = pHmin, pHmax = pHmax,
                                       Rmin = sensor_params[,"Rmin"], Rmax = sensor_params[,"Rmax"],
                                       delta = sensor_params[,"delta"], pKa = sensor_params[,"pKa"],
                                       by = by)

    new_error$Rmin = as.character(rep(sensor_params[,"Rmin"], length(new_error$E)))
    new_error$Rmax = as.character(rep(sensor_params[,"Rmax"], length(new_error$E)))
    new_error$Name = as.character(rep(sensor_params[,"name"], length(new_error$E)))
    new_error %>%  mutate_if(is.factor, as.character) -> new_error

    error_df_full <- rbind(
      error_df_full,
      new_error
    )

  }
  error_df_full
}

#' Creates an error df at multiple inaccuracies, with multiple Rmin/Rmax/delta/pKd parameters
#' @param inaccuracies A vector of inaccuracies (e.g. 0.02 for 2\% error), always relative
#' @param pLigand_min The minimum pH for which to record error
#' @param pLigand_max The maximum pH for which to record error
#' @param param_df A dataframe containing a list of sensor parameters, with these columns:
#' 'name': An identifier for this sensor
#' 'Rmin': the minimum possible ratiometric fluorescence for this sensor
#' 'Rmax': the maximum possible ratiometric fluorescence for this sensor
#' 'delta': the ratiometric fluorescence in the first wavelength for this sensor
#' 'pKd': this sensor's midpoint/pKd
#' @param by (optional, default: 0.01) The granularity of the error table--e.g., by = 0.01 would record 275 and 275.01, etc.
#' @param type
#' @param ligand_name the name of this ligand
#' @return A dataframe of errors with columns:
#' 'Name': this sensor name
#' '(ligand_name)': the pLigand
#' 'Rmin': the minimum possible ratiometric fluorescence for this sensor
#' 'Rmax': the maximum possible ratiometric fluorescence for this sensor
#' 'Error': the error in this pLigand
#' 'Inaccuracy': The inaccuracy of the measurements (relative to R).
#' @examples
#' create_error_df_pLigand_multiple(
#' c(0.01, 0.02), 2, 10,
#' data.frame(
#'     "Rmin" = c(1, 2),
#'     "Rmax" = c(5, 6),
#'     "delta" = c(0.2, 1.2),
#'     "name" = c("normal", "plusOne"),
#'     "pKd" = c(7, 8)
#' ),
#' ligand_name = "NADPH"
#' )
#' @export
create_error_df_pLigand_multiple <- function(inaccuracies, pLigand_min, pLigand_max, param_df, by = 0.01, ligand_name) {
  error_df_full <- data.frame(ligand_name = c(), Rmin = c(), Rmax = c(),
                              Name = c(), Error = c(), Inaccuracy = c())
  # Loop through each sensor in the param_df
  for(n in 1:nrow(param_df)) {
    sensor_params <- param_df[n, ]

    new_error <- create_error_df_pLigand(inaccuracies, pLigand_min = pLigand_min, pLigand_max = pLigand_max,
                                    Rmin = sensor_params[,"Rmin"], Rmax = sensor_params[,"Rmax"],
                                    delta = sensor_params[,"delta"], pKd = sensor_params[,"pKd"],
                                    by = by, ligand_name = ligand_name)

    new_error$Rmin = as.character(rep(sensor_params[,"Rmin"], length(new_error$E)))
    new_error$Rmax = as.character(rep(sensor_params[,"Rmax"], length(new_error$E)))
    new_error$Name = as.character(rep(sensor_params[,"name"], length(new_error$E)))
    new_error %>%  mutate_if(is.factor, as.character) -> new_error

    error_df_full <- rbind(
      error_df_full,
      new_error
    )

  }
  error_df_full
}

# Range dataframes -------------------------------------------------------------

#' Takes in the input of create_error_df_redox_multiple and creates a simple ranges plot:
#' e.g. minimum and maximum measureable value at different error thresholds for each sensor.
#' @param error_df A dataframe of errors at least these columns:
#' 'Name': this sensor name
#' 'E': the redox potential (mV),
#' 'Error': the error in this redox potential (mV)
#' 'Inaccuracy': The inaccuracy of the measurements (relative to R).
#' @param thresholds A vector of error thresholds (e.g. c(0.5, 1) for 0.5mV and 1mV)
#' @param parameter the biochemical value being measured
#' @return A dataframe of suited ranges with these columns:
#' 'Sensor_Name': the name of the sensor
#' 'Minimum': the minimum redox potential (mV) measurable at the given inaccuracy
#' 'Maximum': the maximum redox potential (mV) measurable at the given inaccuracy
#' 'Inaccuracy': the inaccuracy associated with this row (relative)
#' 'error_thresh': the error threshold associated with this row (mV)
#' @examples
#' error_df <- create_error_df_redox_multiple(
#' c(0.02), -400, -200,
#' data.frame(
#'   Rmin = 0.97,
#'   Rmax = 4.12,
#'   delta = 0.23,
#'   name = "roGFP2",
#'   e0 = -299
#' )
#' )
#' create_ranges_multiple(error_df)
#' @import dplyr
#' @export
create_ranges_multiple <- function(error_df, thresholds = c(0.5, 1, 1.5, 2, 2.5), parameter = "E") {
  ranges_df <- data.frame("Sensor_Name" = c(), "Minimum" = c(),
                          "Inaccuracy" = c(), "Maximum" = c(),
                          "error_thresh" = c())
  names <- unique(error_df[,"Name"])
  inaccuracies <- unique(error_df[,"Inaccuracy"])
  for(inaccuracy in inaccuracies) {
    error_df_inaccuracy <- error_df %>% dplyr::filter(Inaccuracy == inaccuracy)
    for(name in names) {
      sensor_error <- error_df_inaccuracy %>% dplyr::filter(Name == name)
      for(thresh in thresholds) {
        within_threshold <- sensor_error %>%
          dplyr::filter(Error <= thresh)
        maximum_value <- suppressWarnings(max(within_threshold[,parameter]))
        minimum_value <- suppressWarnings(min(within_threshold[,parameter]))
        new_df <- data.frame(
          "Sensor_Name" = paste0(name, "_", as.character(inaccuracy)),
          "Minimum" = if(is.finite(minimum_value)) minimum_value else "NA",
          "Maximum" = if(is.finite(maximum_value)) maximum_value else "NA",
          "Inaccuracy" = inaccuracy,
          "error_thresh" = thresh,
          stringsAsFactors = FALSE
        )
        ranges_df <- rbind(
          ranges_df,
          new_df
        )
      }
    }
  }
  ranges_df
}

# Range plotting functions! ----------------------------------------------------

#' Takes in a ranges_df dataframe and makes a plot!
#' @param ranges A dataframe of ranges with at least these columns:
#' 'Sensor_Name': the name of the sensor
#' 'Minimum': the minimum parameter measurable at the given inaccuracy
#' 'Maximum': the maximum parameter measurable at the given inaccuracy
#' 'Inaccuracy': the inaccuracy associated with this row (relative)
#' 'error_thresh': the error threshold associated with this row
#' @param ylim The limits of the ranges plot
#' @param by the 'by' argument of the limits axis tick marks
#' @return A ggplot object
#' @examples
#' @import ggplot2
#' @import RColorBrewer
#' @import cowplot
#' @export
plot_ranges_general <- function(ranges, ylim, by, y_label) {
  ranges$Inaccuracy <- as.numeric(ranges$Inaccuracy)
  suppressWarnings(ranges$Minimum <- as.numeric(ranges$Minimum))
  suppressWarnings(ranges$Maximum <- as.numeric(ranges$Maximum))
  ranges <- ranges[complete.cases(ranges), ]
  ggplot() +
    geom_linerange(data = ranges %>% arrange(-error_thresh),
                    mapping=aes(x = Sensor_Name, ymin = Minimum,
                                ymax = Maximum, lwd = 1, color = error_thresh),
                   size = 10) +
    scale_y_continuous(breaks = seq(ylim[1], ylim[2], by = by)) +
    scale_color_continuous(high = "lightgreen", low = "forestgreen") +
    xlab("") +
    ylab(y_label) +
    theme_classic() +
    theme(aspect.ratio = 1) +
    coord_flip(ylim = ylim)
}

#' Takes in a ranges_df dataframe and makes a plot (for redox).
#' @param ranges A dataframe of ranges with at least these columns:
#' 'Sensor_Name': the name of the sensor
#' 'Minimum': the minimum redox potential (mV) measurable at the given inaccuracy
#' 'Maximum': the maximum redox potential (mV) measurable at the given inaccuracy
#' 'Inaccuracy': the inaccuracy associated with this row (relative)
#' 'error_thresh': the error threshold associated with this row (mV)
#' @param ylim The limits of the ranges plot
#' @param by the 'by' argument of the limits axis tick marks
#' @param ylab The label of the ranges plot
#' @return A ggplot object
#' @examples
#' error_df <- create_error_df_redox_multiple(
#' c(0.02, 0.04), -400, -200,
#' data.frame(
#'   Rmin = 0.97,
#'   Rmax = 4.12,
#'   delta = 0.23,
#'   name = "roGFP2",
#'   e0 = -299
#' )
#' )
#' ranges_df <- create_ranges_multiple(error_df)
#' plot_ranges_redox(ranges_df)
#' @import ggplot2
#' @import RColorBrewer
#' @import cowplot
#' @export
plot_ranges_redox <- function(ranges, ylim = c(-350, -150),
                              by = 20, ylab = "Glutathione Redox Potential (mV)") {
  plot_ranges_general(ranges, ylim, by, y_label = ylab)
}


#' Takes in a ranges_df dataframe and makes a plot (for pH).
#' @param ranges A dataframe of ranges with at least these columns:
#' 'Sensor_Name': the name of the sensor
#' 'Minimum': the minimum pH measurable at the given inaccuracy
#' 'Maximum': the maximum pH measurable at the given inaccuracy
#' 'Inaccuracy': the inaccuracy associated with this row (relative)
#' 'error_thresh': the error threshold associated with this row
#' @param ylim The limits of the ranges plot
#' @param by the 'by' argument of the limits axis tick marks
#' @param ylab The label of the ranges plot
#' @return A ggplot object
#' @examples
#' error_df <- create_error_df_pH_multiple(
#' c(0.01, 0.02), 2, 10,
#' data.frame(
#'   "Rmin" = c(1, 2),
#'   "Rmax" = c(5, 6),
#'   "delta" = c(0.2, 1.2),
#'   "name" = c("normal", "plusOne"),
#'   "pKa" = c(7, 8)
#' )
#' )
#' ranges_df <- create_ranges_multiple(error_df, parameter = "pH",
#' thresholds = c(0.01, 0.05, 0.10, 0.15, 0.20))
#' plot_ranges_pH(ranges_df)
#' @import ggplot2
#' @import RColorBrewer
#' @import cowplot
#' @export
plot_ranges_pH <- function(ranges, ylim = c(1, 14),
                              by = 1, ylab = "pH") {
  plot_ranges_general(ranges, ylim, by, y_label = ylab)
}

#' Takes in a ranges_df dataframe and makes a plot (for pLigand).
#' @param ranges A dataframe of ranges with at least these columns:
#' 'Sensor_Name': the name of the sensor
#' 'Minimum': the minimum pLigand measurable at the given inaccuracy
#' 'Maximum': the maximum pLigand measurable at the given inaccuracy
#' 'Inaccuracy': the inaccuracy associated with this row (relative)
#' 'error_thresh': the error threshold associated with this row
#' @param ylim The limits of the ranges plot
#' @param by the 'by' argument of the limits axis tick marks
#' @param ylab The label of the ranges plot
#' @return A ggplot object
#' @examples
#' error_df <- create_error_df_pLigand_multiple(
#' c(0.01, 0.02), 2, 10,
#' data.frame(
#'   "Rmin" = c(1, 2),
#'   "Rmax" = c(5, 6),
#'   "delta" = c(0.2, 1.2),
#'   "name" = c("normal", "plusOne"),
#'   "pKd" = c(7, 8)
#' ),
#' ligand_name = "NADPH"
#' )
#' ranges_df <- create_ranges_multiple(error_df, parameter = "NADPH",
#' thresholds = c(0.01, 0.05, 0.10, 0.15, 0.20))
#' plot_ranges_pLigand(ranges_df, ylab = "pNADPH")
#' @import ggplot2
#' @import RColorBrewer
#' @import cowplot
#' @export
plot_ranges_pLigand <- function(ranges, ylim = c(1, 14),
                           by = 1, ylab = "pLigand") {
  plot_ranges_general(ranges, ylim, by, y_label = ylab)
}

