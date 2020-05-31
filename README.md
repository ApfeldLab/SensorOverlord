# SensorOverlord

**Build:**
[![Build Status](https://travis-ci.com/ApfeldLab/SensorOverlord.svg?branch=master&status=passed)](https://travis-ci.com/github/ApfeldLab/SensorOverlord)
[![codecov](https://codecov.io/gh/apfeldlab/sensoroverlord/branch/master/graph/badge.svg)](https://codecov.io/gh/apfeldlab/sensoroverlord)

**Documentation:**
[![Docs](https://img.shields.io/badge/Documentation-Reference-green.svg)](https://apfeldlab.github.io/SensorOverlord/)


---------------------


This package aims to help you understand the limitations of quantatative measurements with two-state ratiometric sensors. 

You can load sensor spectra information into an object, take ratio measurements from that spectra, convert those ratio measurements into biophysically-meaningful values (e.g. fraction protenated and pH or fraction oxidized and redox potential), and then analyze how errors in ratio measurements affect the errors in those values. 

## Getting Started

You can install the package from within R with the `devtools` package. 

``` r
install.packages("devtools")
devtools::install_github("julianstanley/SensorOverlord")
```

You can then load the package like any other.

```r
library(sensorOverlord)
```

## Building a sensor

### Generic Sensor

You can build a sensor by one of two methods:

1. By using a spectra, usually provided as a .csv file of excitation-emission values

```r
# Build a spectra object from a csv file
spectra_file <- read.csv("spectra.csv", header = FALSE)
spectra <- spectraMatrixFromValues(
    lambdas_minimum = spectra_file,
    values_minimum = spectra_file,
    lambdas_maximum = spectra_file,
    values_maximum = spectra_file
)

# After building a spectra, I need to specify my ratiometric emission bands
# in this case, I'll specify (410 nm +/- 10 nm) / (470 nm +/- 10 nm)
my_sensor <- newSensorFromSpectra(spectra,
                     lambda_1 = c(400, 420),
                     lambda_2 = c(460, 480))
```


2. By using empirically-determined Rmin, Rmax, and delta values

```r
my_sensor <- new("Sensor", Rmin = 1, Rmax = 5, delta = 0.2)
```

### Specalized sensor

Once you have a generic sensor, you can create a more specalized sensor by passing
specalization-specific parameters.

For example, to make a redox sensor, I need to pass a midpoint potential, e0:

```r
my_redox_sensor <- new("redoxSensor", my_sensor, e0 = -250)
```

## Computing accurate ranges

One of the big outputs of this package is a plot that specifies tha accuracy 
of a given sensor at certain levels of microscopy precision.

To generate that output, you can call plotRanges() on a sensor object.

Right now, that range plot is only enabled for redoxSensors. For other sensors, 
hold tight (or, if you want them sooner, please email me at julianstanleya [at] gmail [dot] com 
and I'll majorly expedite the process!)

For example, here is how you would create that plot for the redox sensor created above,
across microscope inaccuracies ranging from 2% to 8% relative error, at "acceptable error"
thesholds of 1-5mV:

```r
library(sensorOverlord)

# Ranges plot of a sensor at different inaccuracies and thresholds
rangePlot(my_redox_sensor, ranges = ranges_df(my_redox_sensor,
                                        inaccuracies = seq(0.02, 0.08, by = 0.01),
                                        thresholds = 1:5))
```
