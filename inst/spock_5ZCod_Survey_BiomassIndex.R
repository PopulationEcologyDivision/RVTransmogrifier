#############################
# 5Z Cod
# Survey Biomass Index
# July 1, 2021
# Author: Caira Clark
#############################

require(dplyr)
require(ggplot2)
require(here)
require(tidyr)

#Load data. There are two groups depending on how the conversion factor is treated.
biomass <- read.csv(here("data/surveybiomass.csv"))
biomass1 <- biomass %>% filter(year<2009)
biomass2 <- biomass %>% filter(year>2008)


#Convert NMFS Fall and NMFS Spring
biomass1$cnmfsfall <- biomass1$nmfsfall * biomass1$nfallconv
biomass1$cnmfsspr <- biomass1$nmfsspr * biomass1$nsprconv
biomass2$cnmfsfall <- biomass2$nmfsfall / biomass2$nfallconv
biomass2$cnmfsspr <- biomass2$nmfsspr / biomass2$nsprconv

#Bind them together
biomass <- rbind(biomass1, biomass2)

#Calculate standard converted biomass for each survey
biomass1986 <-biomass %>% 
  filter(year > 1985) %>%
  select(year, dfospr, cnmfsfall, cnmfsspr)
biomass1986[is.na(biomass1986)] <- 0

biomass[is.na(biomass)] <- 0
biomass$cnmfsfall_mean <- mean(biomass1986$cnmfsfall)
biomass$cnmfsspr_mean <- mean(biomass1986$cnmfsspr)
biomass$dfospr_mean <- mean(biomass1986$dfospr)

#These are the standard converted biomass calculations
biomass$NMFS.Fall <- biomass$cnmfsfall/biomass$cnmfsfall_mean
biomass$NMFS.Spring <- biomass$cnmfsspr/biomass$cnmfsspr_mean
biomass$DFO <- biomass$dfospr/biomass$dfospr_mean

#Pivot into long format
biomass_long <- biomass %>%
  select(year, NMFS.Fall, NMFS.Spring, DFO) %>%
  pivot_longer(!year, names_to = "Survey", values_to = "biomass")
biomass_long[biomass_long == 0] <- NA

#Plot
ggplot(biomass_long) +
  geom_line(aes(x=year, y=biomass, group=Survey, color=Survey)) +
  geom_point(aes(x=year, y=biomass, group=Survey, colour = Survey)) + 
  theme_bw() +
  xlab("Year") +
  ylab("Scaled Biomass Index") +
  coord_cartesian(ylim=c(0, 10))

ggsave(here("figures/Survey_ScaledBiomassIndex.png"))

#Plot with Fall Adjusted to be in the same year as the two springs. Called 'assessment year'
biomass_long$AssessYear<-with(biomass_long, ifelse(Survey=="NMFS.Fall", year+1, year))
ggplot(biomass_long) +
  geom_line(aes(x=AssessYear, y=biomass, group=Survey, color=Survey)) +
  geom_point(aes(x=AssessYear, y=biomass, group=Survey, colour = Survey)) + 
  theme_bw() +
  xlab("Year of Assessment") +
  ylab("Scaled Biomass Index") +
  coord_cartesian(ylim=c(0, 10))

ggsave(here("figures/Survey_ScaledBiomassIndex_YearAssessed.png"))

