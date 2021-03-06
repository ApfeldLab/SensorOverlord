% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sensor_Methods.R
\name{plotProperty,redoxSensor-method}
\alias{plotProperty,redoxSensor-method}
\title{Plot the E of a redoxSensor}
\usage{
\S4method{plotProperty}{redoxSensor}(object, R = getR(object))
}
\arguments{
\item{object}{An redoxSensor object}

\item{R}{An array of numeric ratio values}
}
\value{
A ggplot object
}
\description{
Creates a ggplot object that has the ratio R on the horizontal axis
and the redox potential (E) on the vertical axis
}
