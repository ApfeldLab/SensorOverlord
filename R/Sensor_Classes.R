# Sensor classes
setClass("Sensor",
         slots =
             list(Rmin = "numeric", Rmax = "numeric", delta = "numeric")
)

setValidity("Sensor",
            function(object) {
                if (object@Rmin >= object@Rmax)
                    "Rmin must be smaller than Rmax"
                if ((object@Rmin < 0) | (object@Rmax < 0) | (object@delta) < 0)
                    "All parameters must be positive"
                else
                    TRUE
            })

# Rmax always represents fully oxidized
# Rmin always represents fully reduced
setClass("redoxSensor",
         slots =
             list(e0 = "numeric"),
         contains = "Sensor"
)

# Rmax always represents fully deprotenated
# Rmin always represents fully protenated
setClass("pHSensor",
         slots =
             list(pKa = "numeric"),
         contains = "Sensor")



