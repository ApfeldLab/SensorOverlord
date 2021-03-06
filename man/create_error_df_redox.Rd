% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{create_error_df_redox}
\alias{create_error_df_redox}
\title{Creates a dataframe of errors in redox potential at given inaccuracies}
\usage{
create_error_df_redox(
  inaccuracies,
  Emin,
  Emax,
  Rmin,
  Rmax,
  delta,
  e0,
  temp = 295.15,
  by = 0.01
)
}
\arguments{
\item{inaccuracies}{A vector of inaccuracies (e.g. 0.02 for 2\% error), always relative}

\item{Emin}{The minimum redox potential, in mV, for which to record error}

\item{Emax}{The maximum redox potential, in mV, for which to record error}

\item{Rmin}{the minimum possible ratiometric fluorescence}

\item{Rmax}{the maximum possible ratiometric fluorescence}

\item{delta}{the ratiometric fluorescence in the first wavelength}

\item{e0}{the sensor's midpoint potential}

\item{temp}{(optional, default: 295.15) the temperature (in Kelvin) at which measurements were made}

\item{by}{(optional, default: 0.01) The granularity of the error table--e.g., by = 0.01 would record 275 and 275.01, etc.}
}
\value{
A dataframe of errors with columns:
'E': the redox potential (mV),
'Error': the error in this redox potential (mV)
'Inaccuracy': The inaccuracy of the measurements (relative to R).
}
\description{
Creates a dataframe of errors in redox potential at given inaccuracies
}
\examples{
create_error_df_redox(c(0.01, 0.02), -300, -200, 1, 5, 0.2, -275)
}
