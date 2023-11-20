library(dplR)
source("Wuchswerte_2.R")

romlimes <- read.rwl("data/ROMLIMES.rwl")

romlimes_hol <- as.rwl(wuchswerte_2(romlimes))

romlimes_glk_list <- glk(romlimes, overlap = 50)
romlimes_hol_glk_list <- glk(romlimes_hol)
romlimes_sgc_list <- sgc(romlimes, overlap = 50)
romlimes_cor <- corr.rwl.seg(romlimes, seg.length = 24, method = "pearson")

mean_glk <- mean(romlimes_glk_list$glk_mat[upper.tri(romlimes_glk_list$glk_mat)], na.rm = TRUE)
mean_p_glk <- mean(romlimes_glk_list$p_mat[upper.tri(romlimes_glk_list$p_mat)], na.rm = TRUE)

mean_sgc <- mean(romlimes_sgc_list$sgc_mat[upper.tri(romlimes_sgc_list$sgc_mat)], na.rm = TRUE)
mean_ssgc <- mean(romlimes_sgc_list$ssgc_mat[upper.tri(romlimes_sgc_list$ssgc_mat)], na.rm = TRUE)
mean_p <- mean(romlimes_sgc_list$p_mat[upper.tri(romlimes_sgc_list$p_mat)], na.rm = TRUE)

mean_cor <- mean(romlimes_cor$spearman.rho, na.rm = TRUE)

write.csv(romlimes_sgc_list$sgc_mat, "export/romlimes_sgc_mat.csv")
write.csv(romlimes_sgc_list$ssgc_mat, "export/romlimes_ssgc_mat.csv")
write.csv(romlimes_sgc_list$p_mat, "export/romlimes_p_mat.csv")
write.csv(romlimes_glk_list$glk_mat, "export/romlimes_glk_mat.csv")
write.csv(romlimes_glk_list$p_mat, "export/romlimes_glk_p_mat.csv")
write.csv(romlimes_cor$spearman.rho, "export/romlimes_r_mat.csv")


