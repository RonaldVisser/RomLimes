library(dplR)
source("segmented_sgc.R")
source("segmented_glk.R")

romlimes <- read.rwl("data/ROMLIMES.rwl")

rom_seg_sgc <- segmented_sgc(romlimes)

romlimes_sgc_segments <- rom_seg_sgc$sgc_segments
romlimes_ssgc_segments <- rom_seg_sgc$ssgc_segments


rom_seg_glk <- segmented_glk(romlimes)
