# values_minimum represents state corresponding to Rmin
# values_maximum represents state corresponding to Rmax
setClass("sensorSpectra",
         slots =
             list(lambdas = "numeric",
                  values_minimum = "numeric",
                  values_maximum = "numeric"
             ))
