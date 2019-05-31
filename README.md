# SensorOverlord

**Build:**
[![Build Status](https://travis-ci.org/julianstanley/SensorOverlord.svg?branch=master)](https://travis-ci.org/julianstanley/SensorOverlord)
[![codecov](https://codecov.io/gh/julianstanley/SensorOverlord/branch/master/graph/badge.svg)](https://codecov.io/gh/julianstanley/SensorOverlord)

**Documentation:**
[![Notebooks](https://img.shields.io/badge/Jupyter%20Notebooks-Interactive%20Package%20Guide-green.svg)](https://github.com/julianstanley/SensorOverlord_Notebooks)

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

