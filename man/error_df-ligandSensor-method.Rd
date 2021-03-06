% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sensor_Methods.R
\name{error_df,ligandSensor-method}
\alias{error_df,ligandSensor-method}
\title{Finds the error df of this ligand sensor at given inaccuracies}
\usage{
\S4method{error_df}{ligandSensor}(
  object,
  inaccuracies = c(0.01),
  pLigand_min = 1,
  pLigand_max = 14,
  by = 0.001,
  name = "Sensor"
)
}
\arguments{
\item{object}{A ligandSensor object}

\item{inaccuracies}{(optional, default: c(0.01)) A vector of inaccuracies
(e.g. 0.02 for 2\% error), always relative}

\item{pLigand_min}{(optional, default: 1)  The minimum pLigand
for which to record error}

\item{by}{(optional, default: 0.001) The granularity of the error table--e.g.,
by = 0.01 would record 7 and 7.01, etc.}

\item{name}{(optional, default: "Sensor") A name for this sensor}

\item{pLigandmax}{(optional, default: 14) The maximum pLigand
for which to record error}
}
\value{
A dataframe of errors with columns:
'Name': this sensor name
'pLigand': the pLigand,
'Rmin': the minimum possible ratiometric fluorescence for this sensor
'Rmax': the maximum possible ratiometric fluorescence for this sensor
'Error': the error in this pLigand
'Inaccuracy': The inaccuracy of the measurements (relative to R).
}
\description{
Adding this method on 31 May 2020, hoping this style will depreciate
getErrorTable in the future.
}
\examples{
my_sensor <- new("ligandSensor", new("Sensor", Rmin = 1, Rmax = 5, delta = 0.2),
pKd = 7, ligand_name = "NADPH")
error_df(my_sensor,
  inaccuracies = c(0.01, 0.02), pLigand_min = 1, pLigand_max = 14,
)
}
