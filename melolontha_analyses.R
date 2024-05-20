library(dplR)
library(ggplot2)
library(reshape2)
library(ggpubr)
source("read.fh.R")
source("moving_window_minimum.R")
source("period_minima.R")

melo3y <- read.fh("data/Melolontha_3yr.fh", BC_correction = TRUE)
melo4y <- read.fh("data/Melolontha_4yr.fh", BC_correction = TRUE)

# function to remove year 0 from dataframe
years_bc_ad <- function(data_with_years) {
  data_with_years$years <- as.integer(rownames(data_with_years))
  data_with_years$years[data_with_years$years<1] <- data_with_years$years[data_with_years$years<1]-1
  data_with_years
}



# Periodicity: 3 years ----------------------------------------------------

melo3y_wind5 <- moving_window_minimum(melo3y, window_size = 5)
melo3y_per_min <- period_minima(melo3y_wind5)

# finding synchronous minima
minima_sync_3 <- as.data.frame(rowSums(melo3y_wind5))
colnames(minima_sync_3) <- "n_minima"
# remove year 0, which does not exist
minima_sync_3 <- years_bc_ad(minima_sync_3)

p_minima_sync_3 <- ggplot(minima_sync_3, aes(x=years, y=n_minima)) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks=c(-150,-100,-50,1,50,100,150)) + ylab("count")
ggsave("export/period_3/sync_minima.png", plot = p_minima_sync_3)

# # remove year 0 for plotting. Year 0 does not exist
melo3y <- years_bc_ad(melo3y)
melo3y_wind5 <- years_bc_ad(melo3y_wind5)


melo3y %>%
  pivot_longer(VDM2W130:VNW00041) %>% 
  ggplot(aes(x=years, y=value, colour = name)) + geom_line() + 
  scale_x_continuous(breaks=c(-150,-100,-50,1,50,100,150)) +  facet_grid(~name)
ggsave("export/period_3/All_series.png")

melo3y %>%
  ggplot(aes(x=years, y=VDM2W130)) + geom_line() +
  geom_vline(xintercept=melo3y_wind5$years[melo3y_wind5$VDM2W130], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-150,-100,-50,1,50,100,150))
ggsave("export/period_3/VDM2W130.png")

melo3y %>%
  ggplot(aes(x=years, y=VDMLR011)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo3y_wind5))[melo3y_wind5$VDMLR011], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-150,-100,-50,1,50,100,150))
ggsave("export/period_3/VDMLR011.png")

melo3y %>%
  ggplot(aes(x=years, y=VDMLR151)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo3y_wind5))[melo3y_wind5$VDMLR151], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-150,-100,-50,1,50,100,150))
ggsave("export/period_3/VDMLR151.png")

melo3y %>%
  ggplot(aes(x=years, y=VNW00021)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo3y_wind5))[melo3y_wind5$VNW00021], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-150,-100,-50,1,50,100,150))
ggsave("export/period_3/VNW00021.png")

melo3y %>%
  ggplot(aes(x=years, y=VNW00031)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo3y_wind5))[melo3y_wind5$VNW00031], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-150,-100,-50,1,50,100,150))
ggsave("export/period_3/VNW00031.png")

melo3y %>%
  ggplot(aes(x=years, y=VNW00041)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo3y_wind5))[melo3y_wind5$VNW00041], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-150,-100,-50,1,50,100,150))
ggsave("export/period_3/VNW00041.png")


# Periodicity: 4 years ----------------------------------------------------

melo4y_wind7 <- moving_window_minimum(melo4y, window_size = 7)
melo4y_per_min <- period_minima(melo4y_wind7)

# 4 year periodicity is stronger with 5 year window
melo4y_wind5 <- moving_window_minimum(melo4y, window_size = 5)
melo4y_per_min_win5 <- period_minima(melo4y_wind5)

# finding synchronous minima
minima_sync_4_w7 <- as.data.frame(rowSums(melo4y_wind7))
colnames(minima_sync_4_w7) <- "n_minima"
# remove year 0, which does not exist
minima_sync_4_w7 <- years_bc_ad(minima_sync_4_w7)

p_minima_sync_4_w7 <- ggplot(minima_sync_4_w7, aes(x=years, y=n_minima)) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks=c(-40,1,40,80,120)) + ylab("count")
ggsave("export/period_4/sync_minima_w7.png", plot = p_minima_sync_4_w7)

# finding synchronous minima
minima_sync_4_w5 <- as.data.frame(rowSums(melo4y_wind5))
colnames(minima_sync_4_w5) <- "n_minima"
# remove year 0, which does not exist
minima_sync_4_w5 <- years_bc_ad(minima_sync_4_w5)

ggplot(minima_sync_4_w5, aes(x=years, y=n_minima)) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks=c(-40,1,40,80,120)) + ylab("count")
ggsave("export/period_4/sync_minima_w5.png")

# # remove year 0 for plotting. Year 0 does not exist
melo4y <- years_bc_ad(melo4y)
melo4y_wind7 <- years_bc_ad(melo4y_wind7)
melo4y_wind5 <- years_bc_ad(melo4y_wind5)


melo4y %>%
  pivot_longer(ALB00010:VNW00101) %>% 
  ggplot(aes(x=years, y=value, colour = name)) + geom_line() + facet_grid(~name) +
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/All_series.png")

melo4y %>%
  ggplot(aes(x=years, y=ALB00010)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo4y_wind7))[melo4y_wind7$ALB00010], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/ALB00010.png")

melo4y %>%
  ggplot(aes(x=years, y=ALB00010)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo4y_wind5))[melo4y_wind5$ALB00010], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/ALB00010_5yrwin.png")

melo4y %>%
  ggplot(aes(x=years, y=IWRD577C)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo4y_wind7))[melo4y_wind7$IWRD577C], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/IWRD577C.png")

melo4y %>%
  ggplot(aes(x=years, y=IWRD577C)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo4y_wind5))[melo4y_wind5$IWRD577C], 
             colour="red", linetype = "dotted" )+
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/IWRD577C_5yrwin.png")


melo4y %>%
  ggplot(aes(x=years, y=VDMWL473)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo4y_wind7))[melo4y_wind7$VDMWL473], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/VDMWL473.png")

melo4y %>%
  ggplot(aes(x=years, y=VDMWL473)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo4y_wind5))[melo4y_wind5$VDMWL473], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/VDMWL473_5yrwin.png")


melo4y %>%
  ggplot(aes(x=years, y=VDMWL53Q)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo4y_wind7))[melo4y_wind7$VDMWL53Q], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/VDMWL53Q.png")

melo4y %>%
  ggplot(aes(x=years, y=VDMWL53Q)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo4y_wind5))[melo4y_wind5$VDMWL53Q], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/VDMWL53Q_5yrwin.png")

melo4y %>%
  ggplot(aes(x=years, y=VNW00101)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo4y_wind7))[melo4y_wind7$VNW00101], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/VNW00101.png")

melo4y %>%
  ggplot(aes(x=years, y=VNW00101)) + geom_line() +
  geom_vline(xintercept=as.integer(row.names(melo4y_wind5))[melo4y_wind5$VNW00101], 
             colour="red", linetype = "dotted" ) +
  scale_x_continuous(breaks=c(-40,1,40,80,120))
ggsave("export/period_4/VNW00101_5yrwin.png")


# Combined 3 and 4 year minima --------------------------------------------


p_minima_combined <- minima_sync_3 %>% 
  left_join(minima_sync_4_w7, by="years", suffix = c(".3yr", ".4yr")) %>% 
  pivot_longer(c("n_minima.3yr","n_minima.4yr")) %>% 
  ggplot(aes(x=years, y=value, fill=name)) + geom_bar(stat = "identity") + 
  scale_x_continuous(breaks=c(-150,-100,-50,1,50,100,150)) + scale_fill_discrete("Synchronous minima") +
  ylab("count")
ggsave("export/minima_combined.png", plot = p_minima_combined, width = 8, height = 3)
  

ggarrange(ggarrange(p_minima_sync_3 + xlab(""), 
                    p_minima_sync_4_w7 + xlab(""), ncol = 2, labels = c("3 year minima", "4 year minima"),
                    font.label = list(size = 12, face = "plain"), hjust = -0.5, vjust = 1.6), 
          p_minima_combined + theme(legend.position="bottom", legend.background = element_blank()), nrow = 2)
ggsave("export/minima_combined_all.png", width = 8, height = 6)

