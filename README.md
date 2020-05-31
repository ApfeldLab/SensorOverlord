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

