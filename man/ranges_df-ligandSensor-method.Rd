% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sensor_Methods.R
\name{ranges_df,ligandSensor-method}
\alias{ranges_df,ligandSensor-method}
\title{Finds the ranges df of this ligand sensor at given inaccuracies}
\usage{
\S4method{ranges_df}{ligandSensor}(
  object,
  inaccuracies = c(0.02),
  pLigand_min = 1,
  pLigand_max = 14,
  by = 0.001,
  name = "Sensor",
  thresholds = c(0.01, 0.05, 0.1, 0.15, 0.2)
)
}
\arguments{
\item{object}{A ligandSensor object}

\item{inaccuracies}{(optional, default: c(0.02)) A vector of inaccuracies
(e.g. 0.02 for 2\% error), always relative}

\item{pLigand_min}{(optional, default: 1)  The minimum pLigand
for which to record error}

\item{pLigand_max}{(optional, default: 14) The maximum pLigand
for which to record error}

\item{by}{(optional, default: 0.001) The granularity of the error table--e.g.,
by = 0.01 would record 7 and 7.01, etc.}

\item{name}{(optional, default: "Sensor") A name for this sensor}

\item{thresholds}{A vector of error thresholds (e.g. c(0.5, 1) for 0.5 and 1)}
}
\value{
A dataframe of suited ranges with these columns:
'Sensor_Name': the name of the sensor
'Minimum': the minimum pLigand measurable at the given inaccuracy
'Maximum': the maximum pLigand measurable at the given inaccuracy
'Inaccuracy': the inaccuracy associated with this row (relative)
'error_thresh': the error threshold associated with this row
}
\description{
Adding this method on 31 May 2020, hoping this style will depreciate
getErrorTable in the future.
}
\examples{
my_sensor <- new("ligandSensor", new("Sensor", Rmin = 1, Rmax = 5, delta = 0.2),
pKd = 7, ligand_name = "NADPH")
ranges_df(my_sensor)
}
