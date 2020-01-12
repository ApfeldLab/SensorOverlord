#!/usr/bin/env Rscript
library("optparse")
library("readr")

# Setup command line arguments
option_list = list(
    make_option(c('-n', "--min"), type = "double",
                default=NULL,
                help = "Rmin value",
                metavar="double"),
    make_option(c('-x', "--max"), type = "double",
               default=NULL,
               help = "Rmax value",
               metavar="double"),
    make_option(c('-d', "--delta"), type = "double",
               default = NULL,
               help = "delta lambda 2 value",
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

