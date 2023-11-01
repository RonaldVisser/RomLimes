library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(dplR)
source("read.fh.R")
source("euclid_dist_trw.R")


radii_samples_similarity <- read_xlsx("data/radii_samples_similarity.xlsx")

summary(radii_samples_similarity)

ggplot(radii_samples_similarity, aes(x=r)) + geom_boxplot()

radii_sim_melt <- melt(radii_samples_similarity, id.vars=c("Series_A", "Radius_A", "Series_B", "Radius_B") , 
                   measure.vars=c("r", "r_wuchs", "t", "t_wuchs", "SGC", "SSGC", "p", "GLK", "p_GLK"))

ggplot(radii_sim_melt) + geom_boxplot(aes(x=value), fill="dodgerblue2") + facet_wrap(~variable, scales="free_x")
ggsave("export/boxplot_trees.png", width = 8, height = 8)


VDM2W_QT1 <- read.fh("data/VDM2W_QT1.fh", BC_correction = TRUE)
sgc(VDM2W_QT1)
mean(euclid_dist_trw(VDM2W_QT1)$mean_ed)


VDML2_QT1 <- read.fh("data/VDML2_QT1.fh", BC_correction = TRUE)
sgc(VDML2_QT1)
mean(euclid_dist_trw(VDML2_QT1)$mean_ed)

VDMLR_QT1 <- read.fh("data/VDMLR_QT1.fh", BC_correction = TRUE)
sgc(VDMLR_QT1)
mean(euclid_dist_trw(VDMLR_QT1)$mean_ed)

VEN_QT1 <- read.fh("data/VEN_QT1.fh", BC_correction = TRUE)
mean(euclid_dist_trw(VEN_QT1)$mean_ed)

VEN_SGC <- sgc(VEN_QT1)$sgc_mat
mean(VEN_SGC[lower.tri(VEN_SGC)])
VEN_SGC_p <- sgc(VEN_QT1)$p_mat
mean(VEN_SGC_p[lower.tri(VEN_SGC_p)])










