% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{create_error_df_redox_multiple}
\alias{create_error_df_redox_multiple}
\title{Creates an error df at multiple inaccuracies, with multiple Rmin/Rmax/delta/e0 parameters}
\usage{
create_error_df_redox_multiple(
  inaccuracies,
  Emin,
  Emax,
  param_df,
  temp = 295.15,
  by = 0.01
)
}
\arguments{
\item{inaccuracies}{A vector of inaccuracies (e.g. 0.02 for 2\% error), always relative}

\item{Emin}{The minimum redox potential, in mV, for which to record error}

\item{Emax}{The maximum redox potential, in mV, for which to record error}

\item{param_df}{A dataframe containing a list of sensor parameters, with these columns:
'name': An identifier for this sensor
'Rmin': the minimum possible ratiometric fluorescence for this sensor
'Rmax': the maximum possible ratiometric fluorescence for this sensor
'delta': the ratiometric fluorescence in the first wavelength for this sensor
'e0': this sensor's midpoint potential}

\item{temp}{(optional, default: 295.15) the temperature (in Kelvin) at which measurements were made}

\item{by}{(optional, default: 0.01) The granularity of the error table--e.g., by = 0.01 would record 275 and 275.01, etc.}
}
\value{
A dataframe of errors with columns:
'Name': this sensor name
'E': the redox potential (mV),
'Rmin': the minimum possible ratiometric fluorescence for this sensor
'Rmax': the maximum possible ratiometric fluorescence for this sensor
'Error': the error in this redox potential (mV)
'Inaccuracy': The inaccuracy of the measurements (relative to R).
}
\description{
Creates an error df at multiple inaccuracies, with multiple Rmin/Rmax/delta/e0 parameters
}
\examples{
create_error_df_redox_multiple(
  c(0.01, 0.02), -300, -200,
  data.frame(
    "Rmin" = c(1, 2),
    "Rmax" = c(5, 6),
    "delta" = c(0.2, 1.2),
    "name" = c("normal", "plusOne"),
    "e0" = c(-275, -274)
  )
)
}
