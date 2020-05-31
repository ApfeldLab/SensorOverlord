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
  R <- R_of_E(E, Rmin, Rmax, delta, e0, temp)
  larger_E <- suppressWarnings(E(R + error_R(R), Rmin, Rmax, delta, e0, temp))
  larger_E[is.nan(larger_E)] <- Inf
  smaller_E <- suppressWarnings(E(R - error_R(R), Rmin, Rmax, delta, e0, temp))
  smaller_E[is.nan(smaller_E)] <- Inf
  return(list(
    E = E,
    larger_E = larger_E,
    smaller_E = smaller_E,
    max_error = pmax(abs(E - larger_E), abs(E - smaller_E))
  ))
}

#' Creates a dataframe of errors at given inaccuracies
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
  error_df_full <- data.frame(E = c(), Error = c(), Inaccuracy = c())
  for (inaccuracy in inaccuracies) {
    error <- Error_E(
      E = seq(Emin, Emax, by = by),
      Rmin = Rmin, Rmax = Rmax, delta = delta, e0 = e0,
      error_R = function(x) inaccuracy * x, temp = temp
    )
    error_df_full <- rbind(
      error_df_full,
      data.frame(
        E = error$E,
        Error = error$max_error,
        Inaccuracy = as.character(rep(inaccuracy, length(error$E)))
      )
    )
  }
  error_df_full
}

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
    for(inaccuracy in inaccuracies) {
        error <- Error_E(
            E = seq(Emin, Emax, by = by),
            Rmin = sensor_params[,"Rmin"], Rmax = sensor_params[,"Rmax"],
            delta = sensor_params[,"delta"], e0 = sensor_params[,"e0"],
            error_R = function(x) inaccuracy * x
        )
        error_df_full <- rbind(
            error_df_full,
            data.frame(
                E = error$E,
                Rmin = as.character(rep(sensor_params[,"Rmin"], length(error$E))),
                Rmax = as.character(rep(sensor_params[,"Rmax"], length(error$E))),
                Name = as.character(rep(sensor_params[,"name"], length(error$E))),
                Error = error$max_error,
                Inaccuracy = as.character(rep(inaccuracy, length(error$E))),
                stringsAsFactors = FALSE
            )
        )
    }
  }
  error_df_full
}

#' Takes in the input of create_error_df_redox_multiple and creates a simple ranges plot:
#' e.g. minimum and maximum measureable value at different error thresholds for each sensor.
#' @param error_df A dataframe of errors at least these columns:
#' 'Name': this sensor name
#' 'E': the redox potential (mV),
#' 'Error': the error in this redox potential (mV)
#' 'Inaccuracy': The inaccuracy of the measurements (relative to R).
#' @param thresholds A vector of error thresholds (e.g. c(0.5, 1) for 0.5mV and 1mV)
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
create_ranges_multiple <- function(error_df, thresholds = c(0.5, 1, 1.5, 2, 2.5)) {
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
        maximum_value <- suppressWarnings(max(within_threshold[,"E"]))
        minimum_value <- suppressWarnings(min(within_threshold[,"E"]))
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

#' Takes in a ranges_df dataframe and makes a plot!
#' @param ranges A dataframe of ranges with at least these columns:
#' 'Sensor_Name': the name of the sensor
#' 'Minimum': the minimum redox potential (mV) measurable at the given inaccuracy
#' 'Maximum': the maximum redox potential (mV) measurable at the given inaccuracy
#' 'Inaccuracy': the inaccuracy associated with this row (relative)
#' 'error_thresh': the error threshold associated with this row (mV)
#' @param ylim The limits of the ranges plot
#' @param by the 'by' argument of the limits axis tick marks
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
plot_ranges_redox <- function(ranges, ylim = c(-350, -150), by = 20) {
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
    ylab("Glutathione Redox Potential (mV)") +
    theme_classic() +
    theme(aspect.ratio = 1) +
    coord_flip(ylim = c(-350, -150))
}

