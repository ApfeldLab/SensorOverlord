#!/usr/bin/env Rscript
library("optparse")
library("readr")
library("sensorOverlord")

# Setup command line arguments
option_list = list(
    make_option(c('-i', "--input"), type = "character",
                default = "spectra.csv",
                help = "Input spectra file",
                metavar = "character"),
    make_option(c('-1', "--lambda1"), type = "double",
                default = 410,
                help = "midpoint of lambda 1",
                metavar = "double"),
    make_option(c('-a', "--lambda1width"), type = "double",
                default = 20,
                help = "1/2 bandwidth of lambda 1",
                metavar = "double"),
    make_option(c('-2', "--lambda2"), type = "double",
                default = 470,
                help = "midpoint of lambda 2",
                metavar = "double"),
    make_option(c('-b', "--lambda2width"), type = "double",
                default = 20,
                help = "1/2 bandwidth of lambda 2",
                metavar = "double"),
    make_option(c('-m', "--midpoint"), type = "double",
                default = 1,
                help = "midpoint value",
                metavar = "double"),
    make_option(c('-e', "--error"), type = "double",
                default = 0.02,
                help = "Relative error",
                metavar = "double"),
    make_option(c('-q', "--emax"), type = "double",
                default = -100,
                help = "maximum E to calculate error",
                metavar = "double"),
    make_option(c('-w', "--emin"), type = "double",
                default = -400,
                help = "maximum E to calculate error",
                metavar = "double"),
    make_option(c('-o', "--out"), type = "character",
                default = paste("error_table_",
                                format(Sys.time(), "%m%e%y_%H%M%S"),
                                ".csv", sep = ""),
                help = "outfile name",
                metavar = "character")
)

# Grab our command line input in opt
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)
opt$out <- gsub(".csv", paste("_",opt$lambda1,
                              "_", opt$lambda2, ".csv", sep = ""), opt$out)

# Get the spectra data
spectra_table <- read.csv(opt$input, header = FALSE)
spectra_1 <- spectra_table$V2
spectra_2 <- spectra_table$V4

# Set the minimum and maximum, based on which one is most consistently
# larger than the other in the first 20 entries.
if(sum(spectra_1[0:20] > spectra_2[0:20]) >
   sum(spectra_2[0:20] > spectra_1[0:20])) {
    spectra_max <- spectra_1
    lambdas_maximum = spectra_table$V1
    spectra_min <- spectra_2
    lambdas_minimum = spectra_table$V3
} else {
    spectra_max <- spectra_2
    lambdas_maximum = spectra_table$V3
    spectra_min <- spectra_1
    lambdas_minimum = spectra_table$V1
}

# Generate a spectra based on the lambda params given
spectra <- spectraMatrixFromValues(
    lambdas_minimum = lambdas_minimum,
    values_minimum = spectra_min,
    lambdas_maximum = lambdas_maximum,
    values_maximum = spectra_max
)

# Generate a sensor
sensor <- new("redoxSensor", newSensorFromSpectra(spectra,
                lambda_1 = c(opt$lambda1 - opt$lambda1width,
                             opt$lambda1 + opt$lambda1width),
                lambda_2 = c(opt$lambda2 - opt$lambda2width,
                             opt$lambda2 + opt$lambda2width)),
              e0 = opt$midpoint)

opt$min <- sensor@Rmin
opt$max <- sensor@Rmax
opt$delta <- sensor@delta

print(paste("Rmin:", opt$min))
print(paste("Rmax:", opt$max))
print(paste("Delta:", opt$delta))
print(paste("E0:", sensor@e0))

# Make the error table

# Define some helpful functions
E <- function(R, Rmin, Rmax, delta, e0, temp = 295.15) {
    return(e0 - (8.315 * temp) / (2 * 96.48104) *
               log( (delta * (Rmax - R)) / (R - Rmin))
    )
}

# Returns R, given params and E
R_of_E <- function(E, Rmin, Rmax, delta, e0, temp = 295.15) {
    return(

        (
            Rmin * exp(((e0 - E) * 2 * 96.48104)/(8.315*temp)) + Rmax
        )
        /
            (
                exp(((e0 - E) * 2 * 96.48104)/(8.315*temp)) + delta
            )
    )
}

# error_R: a function that takes an R and returns the error
Error_E <- function(E, Rmin, Rmax, delta, e0, error_R, temp = 295.15) {
    R <- R_of_E(E, Rmin, Rmax, delta, e0, temp)
    larger_E <- suppressWarnings(E(R + error_R(R), Rmin, Rmax, delta, e0, temp))
    larger_E[is.nan(larger_E)] <- Inf
    smaller_E <- suppressWarnings(E(R - error_R(R), Rmin, Rmax, delta, e0, temp))
    smaller_E[is.nan(smaller_E)] <- Inf
    return(list(E = E,
                Max_Error = pmax(abs(E - larger_E), abs(E - smaller_E))))
}

# Get our error in E
error <- Error_E(E = seq(opt$emin,opt$emax, by = 0.01),
                 Rmin = opt$min, Rmax = opt$max, delta = opt$delta,
                 e0 = opt$midpoint,
                 error_R = function(x){return(opt$error * x)}
)

write.csv((data.frame(E = error$E, Error = error$Max_Error)), file = opt$out,
          row.names = FALSE)




