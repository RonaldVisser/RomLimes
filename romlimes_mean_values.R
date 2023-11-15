library(dplR)

romlimes <- read.rwl("data/ROMLIMES.rwl")

romlimes_glk_list <- glk(romlimes)
romlimes_sgc_list <- sgc(romlimes)

#romlimes_glk_list$glk_mat
#romlimes_glk_list$overlap
#romlimes_glk_list$p_mat

mean(romlimes_glk_list$glk_mat[upper.tri(romlimes_glk_list$glk_mat)], na.rm = TRUE)
mean(romlimes_glk_list$p_mat[upper.tri(romlimes_glk_list$p_mat)], na.rm = TRUE)
View(romlimes_glk_list$glk_mat)

mean(romlimes_sgc_list$sgc_mat[upper.tri(romlimes_sgc_list$sgc_mat)], na.rm = TRUE)
mean(romlimes_sgc_list$ssgc_mat[upper.tri(romlimes_sgc_list$ssgc_mat)], na.rm = TRUE)

