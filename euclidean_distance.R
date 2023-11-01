#library(dplR)
#library(devtools)
#library(matrixStat)
source("euclid_dist_trw.R")
#source_url("https://raw.githubusercontent.com/AndyBunn/dplR/master/R/read.fh.R") 
source("read.fh.R")

# load data

radii <- read.fh("data/AlleGedateerdeRadialen.fh", BC_correction = TRUE)

radii_ed <- euclid_dist_trw(radii, last_digit_radius = TRUE)




      