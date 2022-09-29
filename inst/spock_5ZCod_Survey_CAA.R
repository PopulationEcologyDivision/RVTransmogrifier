#############################
# 5Z Cod
# Survey Catch at Age
# July 1, 2021
# Author: Caira Clark
#############################

require(ggplot2)
require(dplyr)
require(tidyr)
require(here)

##DFO Spring Survey----------------

#Load the DFO CAA file
CAA_dfo <- read.csv(here("data/Survey CAA/dfo_survey_caa.csv"))

#Calculate 10+ group
CAA_dfo$'a10+' <- CAA_dfo$a10 + CAA_dfo$a11 + CAA_dfo$a12 + CAA_dfo$a13 + CAA_dfo$a14 + CAA_dfo$a15 + CAA_dfo$a16

#Select columns and tidy data into long format
CAA_dfo2 <- CAA_dfo %>% select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Abundance")

#Finish tidying
CAA_dfo2[is.na(CAA_dfo2)] <- 0
CAA_dfo2$Age <- sub("a", "", CAA_dfo2$Age)
CAA_dfo2$Age[which(CAA_dfo2$Age == "10+")] = 10
CAA_dfo2$Age <- as.numeric(CAA_dfo2$Age)

#Calculate the total abundance for each year, and then use that to calculate proportion
CAA_dfo2 <- CAA_dfo2 %>% group_by(Year) %>% mutate(Year_Total = sum(Abundance))
CAA_dfo2$Proportion <- CAA_dfo2$Abundance/CAA_dfo2$Year_Total

#Take out zeros for the plots
CAA_dfo2[CAA_dfo2 == 0] <- NA

#Abundance Plot
ggplot(CAA_dfo2, aes(x=Age, y=Year, size = Abundance)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  lims(y=c(2023, 1970)) + 
  ggtitle("DFO Survey Catch at Age - Abundance")

ggsave(here("figures/DFOSurvey_CAA_Abundance.png")) #This looks better if the height is adjusted either in the plot viewer or in the function

#Proportion Plot

ggplot(CAA_dfo2, aes(x=Age, y=Year, size = Proportion)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  lims(y=c(2023, 1970)) + 
  ggtitle("DFO Survey Catch at Age - Proportion")

ggsave(here("figures/DFOSurvey_CAA_Proportion.png")) #This looks better if the height is adjusted either in the plot viewer or in the function

#NMFS Spring Survey------------- 

#Load the NMFS CAA file
CAA_nmfsspr <- read.csv(here("data/Survey CAA/nmfsspr_survey_caa.csv"))

#Load the NMFS conversion file and calculate spring conversion factors
nmfsconv <- read.csv(here("data/Survey CAA/nmfs_conversionfactors.csv"))
nmfsconv$SpringConversion <- nmfsconv$SpringDoorEffect*nmfsconv$SpringVesselEffect
sprconv <- cbind(nmfsconv$Year, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion,nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion) #There's probably a better way of doing this

#Format the two tables so that they have the same row names and columns
head(CAA_nmfsspr)
sprconv <- as.data.frame(sprconv)
colnames(sprconv) <- c("Year", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16")
head(sprconv)

#Pivot dataframes so that they can be joined easily, and join them
CAA_nmfsspr <- CAA_nmfsspr %>% pivot_longer(!Year, names_to="Age", values_to="Value")
sprconv <- sprconv %>% pivot_longer(!Year, names_to="Age", values_to="cValue")
CAA_nmfsspr$cValue <- CAA_nmfsspr$Value*sprconv$cValue

#Pivot converted data back into long format
CAA_nmfsspr <- CAA_nmfsspr %>% select(Year, Age, cValue) %>% pivot_wider(Year, names_from = Age, values_from = cValue)

#Calculate 10+ group
CAA_nmfsspr$'a10+' <- CAA_nmfsspr$a10 + CAA_nmfsspr$a11 + CAA_nmfsspr$a12 + CAA_nmfsspr$a13 + CAA_nmfsspr$a14 + CAA_nmfsspr$a15 + CAA_nmfsspr$a16

#Select columns and tidy data into long format
CAA_nmfsspr2 <- CAA_nmfsspr %>% select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Abundance")

#Finish tidying
CAA_nmfsspr2[is.na(CAA_nmfsspr2)] <- 0
CAA_nmfsspr2$Age <- sub("a", "", CAA_nmfsspr2$Age)
CAA_nmfsspr2$Age[which(CAA_nmfsspr2$Age == "10+")] = 10
CAA_nmfsspr2$Age <- as.numeric(CAA_nmfsspr2$Age)

#Calculate the total abundance for each year, and then use that to calculate proportion
CAA_nmfsspr2 <- CAA_nmfsspr2 %>% group_by(Year) %>% mutate(Year_Total = sum(Abundance))
CAA_nmfsspr2$Proportion <- CAA_nmfsspr2$Abundance/CAA_nmfsspr2$Year_Total

#Take out zeros for the plots
CAA_nmfsspr2[CAA_nmfsspr2 == 0] <- NA

#Abundance Plot
ggplot(CAA_nmfsspr2, aes(x=Age, y=Year, size = Abundance)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  lims(y=c(2023, 1970)) + 
  ggtitle("NMFS Spring Catch at Age - Abundance")

ggsave(here("figures/NMFSSpring_CAA_Abundance.png")) #This looks better if the height is adjusted either in the plot viewer or in the function

#Proportion Plot

ggplot(CAA_nmfsspr2, aes(x=Age, y=Year, size = Proportion)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  lims(y=c(2023, 1970)) + 
  ggtitle("NMFS Spring Catch at Age - Proportion")

ggsave(here("figures/NMFSSpring_CAA_Proportion.png")) #This looks better if the height is adjusted either in the plot viewer or in the function


#NMFS Fall Survey----------------

#Load the NMFS CAA file
CAA_nmfsfall <- read.csv(here("data/Survey CAA/nmfsfall_survey_caa.csv"))

#Load the NMFS conversion file and calculate spring conversion factors
nmfsconv <- read.csv(here("data/Survey CAA/nmfs_conversionfactors.csv"))
nmfsconv$FallConversion <- nmfsconv$FallDoorEffect*nmfsconv$FallVesselEffect
fallconv <- cbind(nmfsconv$Year, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion,nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion) #There's probably a better way of doing this

#Format the two tables so that they have the same row names and columns
head(CAA_nmfsfall)
fallconv <- as.data.frame(fallconv)
colnames(fallconv) <- c("Year", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16")
head(fallconv)

#Pivot dataframes so that they can be joined easily
CAA_nmfsfall <- CAA_nmfsfall %>% pivot_longer(!Year, names_to="Age", values_to="Value")
fallconv <- fallconv %>% pivot_longer(!Year, names_to="Age", values_to="cValue")
CAA_nmfsfall$cValue <- CAA_nmfsfall$Value*fallconv$cValue

#Pivot converted data back into wide format
CAA_nmfsfall <- CAA_nmfsfall %>% select(Year, Age, cValue) %>% pivot_wider(Year, names_from = Age, values_from = cValue)

#Calculate 10+ group
CAA_nmfsfall$'a10+' <- CAA_nmfsfall$a10 + CAA_nmfsfall$a11 + CAA_nmfsfall$a12 + CAA_nmfsfall$a13 + CAA_nmfsfall$a14 + CAA_nmfsfall$a15 + CAA_nmfsfall$a16

#Select columns and tidy data into long format
CAA_nmfsfall2 <- CAA_nmfsfall %>% select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Abundance")

#Finish tidying
CAA_nmfsfall2[is.na(CAA_nmfsfall2)] <- 0
CAA_nmfsfall2$Age <- sub("a", "", CAA_nmfsfall2$Age)
CAA_nmfsfall2$Age[which(CAA_nmfsfall2$Age == "10+")] = 10
CAA_nmfsfall2$Age <- as.numeric(CAA_nmfsfall2$Age)

#Calculate the total abundance for each year, and then use that to calculate proportion
CAA_nmfsfall2 <- CAA_nmfsfall2 %>% group_by(Year) %>% mutate(Year_Total = sum(Abundance))
CAA_nmfsfall2$Proportion <- CAA_nmfsfall2$Abundance/CAA_nmfsfall2$Year_Total

#Take out zeros for the plots
CAA_nmfsfall2[CAA_nmfsfall2 == 0] <- NA

#Abundance Plot
ggplot(CAA_nmfsfall2, aes(x=Age, y=Year, size = Abundance)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  lims(y=c(2023, 1970)) + 
  ggtitle("NMFS Fall Catch at Age - Abundance")

ggsave(here("figures/NMFSFall_CAA_Abundance.png")) #This looks better if the height is adjusted either in the plot viewer or in the function

#Proportion Plot

ggplot(CAA_nmfsfall2, aes(x=Age, y=Year, size = Proportion)) +
  geom_point(aes(fill= "#00AFBB"), alpha=0.5, colour = "black", pch=21) +
  scale_y_reverse(breaks = scales::pretty_breaks (n=6)) + scale_size(range = c(0.1, 16), name="Abundance (000s)")+
  scale_x_continuous (breaks = scales::pretty_breaks (n=12)) + theme(legend.position = "none") +
  theme (axis.title = element_text (size = 10, colour = "black"), axis.text = element_text (size = 8, colour = "black"), legend.text = element_text (size = 8)) +
  theme(axis.line = element_line(color="black", size = 0.5), panel.background = element_blank(), legend.key=element_blank())+
  lims(y=c(2023, 1970)) + 
  ggtitle("NMFS Fall Catch at Age - Proportion")

ggsave(here("figures/NMFSFall_CAA_Proportion.png")) #This looks better if the height is adjusted either in the plot viewer or in the function
