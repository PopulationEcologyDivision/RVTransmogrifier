#############################
# 5Z Cod
# Survey CV Plots
# July 1, 2021
# Author: Caira Clark
#############################

require(here)
require(ggplot2)
require(dplyr)
require(tidyr)

#Data derived from Bigelow_calibration_CV_calculator_cod_final.xlsx
#And 2021 Update Survey SE and CV_no links.xlsx
dat <- read.csv(here('data/cv_surveys.csv'))

#DFO

df <- dat[dat$survey=='dfo.s',]
colnames(df)[2] <- "CV"
colnames(df) [3] <- "Mean Number/Tow"

df2 <- df %>% 
  pivot_longer(!c(year, survey), names_to = "calc", values_to = "value")

ggplot(df2, aes(year, value, colour=calc)) +
  geom_line() +
  facet_grid(calc~., scales = "free") +
  theme_bw() +
  ylab("Value") +
  xlab("Year") +
  theme(legend.position = "none") +
  ggtitle("DFO Spring")

ggsave(here("figures/DFOSurvey_CV.png"))

#NMFS Spring

df <- dat[dat$survey=='nmfs.s',]
colnames(df)[2] <- "CV"
colnames(df) [3] <- "Mean Number/Tow"

df2 <- df %>% 
  pivot_longer(!c(year, survey), names_to = "calc", values_to = "value")

ggplot(df2, aes(year, value, colour=calc)) +
  geom_line() +
  facet_grid(calc~., scales = "free") +
  theme_bw() +
  ylab("Value") +
  xlab("Year") +
  theme(legend.position = "none") +
  ggtitle("NMFS Spring")

ggsave(here("figures/NMFSSpringSurvey_CV.png"))

#NMFS Fall

df <- dat[dat$survey=='nmfs.f',]
colnames(df)[2] <- "CV"
colnames(df) [3] <- "Mean Number/Tow"

df2 <- df %>% 
  pivot_longer(!c(year, survey), names_to = "calc", values_to = "value")

ggplot(df2, aes(year, value, colour=calc)) +
  geom_line() +
  facet_grid(calc~., scales = "free") +
  theme_bw() +
  ylab("Value") +
  xlab("Year") +
  theme(legend.position = "none") +
  ggtitle("NMFS Fall")

ggsave(here("figures/NMFSFallSurvey_CV.png"))
