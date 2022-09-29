#############################
# 5Z Cod
# Survey Length Frequency Distribution
# July 1, 2021
# Author: Caira Clark
#############################

require(ggplot2)
require(here)
require(reshape2)

##DFO Spring-----------------------

#This is the old script. Now it all works off of LF_ByYear_DFO.csv, including calculating the long term average. Hopefully it's better, but I left the old one here just in case....

#Data for length frequencies derived from STRANAL outputs using the 'Length Total' tabs. It is just a selection of all of the quarters for landings and discards in LF, then output > copy to clipboard, and then sum across the various quarters. 

#Longterm mean based on average number per length over the previous 10 years. 
#LF <- read.csv(here('data/dfo_len_freq_2020_2021_long.csv'))
#LF$Number <- LF$Number/1000
#LF$Longterm <- LF$Longterm/1000

#lf_dfo <- ggplot(LF, aes(Length, Number, group=Year, fill=as.factor(Year))) + geom_bar(stat = "identity", position=position_dodge(width = 1.5), alpha=0.6)+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_y_continuous(name="Abundance (thousands)", breaks = seq(0,700,100)) + scale_x_continuous(name="Length(cm)",breaks = seq(10, 118, 5)) + theme(legend.position = c(0.9,0.8))+ theme(text=element_text(size=14)) + theme(legend.text = element_text(size=14)) + scale_fill_discrete(name="Year")+ geom_line(aes(Flen, Longterm), linetype=2,size=1) + ggtitle("DFO Spring")
#lf_dfo

#ggsave(here("figures/DFOSpring_Length_Frequency.png"))

##DFO Spring-----------------------

#Generates its own longterm average. Just needs a new line added to LF_byYear_DFO.csv/
#Longterm mean based on average number per length over the previous 10 years. 
LF <- read.csv(here('data/LF_ByYear_DFO.csv'))
terminalYear<-max(LF$Year)
TenYrAveYears<-subset(LF, Year<(terminalYear-1)&Year>(terminalYear-1-10)) #If survey is missing, it will only be a nine year average.
TenYrAveYears<-melt(TenYrAveYears, id.vars=c('Year'))
TenYrAveYears$variable<-as.numeric(with(TenYrAveYears, substring(variable,2)))#Removes le X.
TenYrAve<-aggregate(value~variable, FUN="mean", TenYrAveYears)
TenYrAve$Year<-paste(terminalYear-1-11, terminalYear-2, sep="-")

RecentTwoYears<-subset(LF, Year>(terminalYear-2))
RecentTwoYears<-melt(RecentTwoYears, id.vars=c('Year'))
RecentTwoYears$variable<-as.numeric(with(RecentTwoYears, substring(variable,2)))

lf_dfo<-ggplot(RecentTwoYears, aes(variable, value/1000, group=Year, fill=as.factor(Year))) + geom_bar(stat = "identity", position=position_dodge(), alpha=0.6)+ theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_y_continuous(name="Abundance (thousands)") + scale_x_continuous(name="Length(cm)",breaks = seq(0, 118, 5)) + theme(legend.position = c(0.8,0.8))+ theme(text=element_text(size=14)) + theme(legend.text = element_text(size=14))+ scale_fill_discrete(name="Year")+ geom_line(data=TenYrAve, aes(variable, value/1000, col=Year),linetype=2,size=1) + ggtitle("DFO Spring")+ guides(col="none", fill=guide_legend(title="Year"))+scale_fill_viridis_d()+scale_colour_viridis_d()
lf_dfo

ggsave(here("figures/DFOSpring_Length_Frequency.png"), width=7.5, height=4.1, units="in")




##NMFS Fall---------------------

#Data for length frequencies derived from STRANAL outputs using the 'Length Total' tabs.
#Longterm mean based on average number per length over the previous 10 years. This is in the file nmfs_fall_len_freq_longtermcalc.csv
#LF <- read.csv(here('data/nmfs_fall_len_freq_2019_2021_longmb.csv'))
#LF$Total<-colSums(LF,LF$Number)

#LF$Number <- LF$Number/1000
#LF$Longterm <- LF$Longterm/1000

#lf_nmfsfall <- ggplot(LF, aes(Length, Number, group=Year, fill=as.factor(Year))) + geom_bar(stat = "identity", position=position_dodge(width = 1.5), alpha=0.6)+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_y_continuous(name="Abundance (thousands)", limits = c(0,200), breaks = seq(0,200,50)) + scale_x_continuous(name="Length(cm)",breaks = seq(10, 90, 5)) + theme(legend.position = c(0.9,0.8))+ theme(text=element_text(size=14)) + theme(legend.text = element_text(size=14)) + scale_fill_discrete(name="Year")+ geom_line(aes(Flen, Longterm), linetype=2,size=1) +ylim(0, 200) +ggtitle("NMFS Fall")
#lf_nmfsfall

#ggsave(here("figures/NMFSFall_Length_Frequency.png"))

#New Code:
#Generates its own longterm average. Just needs a new line added to LF_byYear_DFO.csv/
#Longterm mean based on average number per length over the previous 10 years. 
LF <- read.csv(here('data/LF_ByYear_NMFSfall.csv'))
terminalYear<-max(LF$Year)
TenYrAveYears<-subset(LF, Year<(terminalYear-1)&Year>(terminalYear-1-10)) #If survey is missing, it will only be a nine year average.
TenYrAveYears<-melt(TenYrAveYears, id.vars=c('Year'))
TenYrAveYears$variable<-as.numeric(with(TenYrAveYears, substring(variable,2)))#Removes le X.
TenYrAve<-aggregate(value~variable, FUN="mean", TenYrAveYears)
TenYrAve$Year<-paste(terminalYear-1-11, terminalYear-2, sep="-")

RecentTwoYears<-subset(LF, Year%in%c(terminalYear, terminalYear-2)) #JUST THIS YEAR (2020 is missing)
#RecentTwoYears<-subset(LF, Year>(terminalYear-2)) #IN A NORMAL YEAR (NO MISSING SURVEY)
RecentTwoYears<-melt(RecentTwoYears, id.vars=c('Year'))
RecentTwoYears$variable<-as.numeric(with(RecentTwoYears, substring(variable,2)))

lf_nmfsfall<-ggplot(RecentTwoYears, aes(variable, value/1000, group=Year, fill=as.factor(Year))) + geom_bar(stat = "identity", position=position_dodge(), alpha=0.6)+ theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_y_continuous(name="Abundance (thousands)") + scale_x_continuous(name="Length(cm)",breaks = seq(0, 118, 5)) + theme(legend.position = c(0.8,0.8))+ theme(text=element_text(size=14)) + theme(legend.text = element_text(size=14))+ scale_fill_discrete(name="Year")+ geom_line(data=TenYrAve, aes(variable, value/1000, col=Year),linetype=2,size=1) + ggtitle("NMFS Fall")+ guides(col="none", fill=guide_legend(title="Year"))+scale_fill_viridis_d()+scale_colour_viridis_d()
lf_nmfsfall

ggsave(here("figures/NMFSFall_Length_Frequency.png"), width=7.5, height=4.1, units="in")



####NMFS Spring Catch at Length

#Data for length frequencies derived from STRANAL outputs using the 'Length Total' tabs.
#Longterm mean based on average number per length over the previous 10 years.
#LF <- read.csv(here('data/nmfs_spring_len_freq_2021_2022_long.csv'))

#LF$Number <- LF$cNumber/1000
#LF$Longterm <- LF$Longterm/1000

#lf_nmfsspr <- ggplot(LF, aes(Length, Number, group=Year, fill=as.factor(Year))) + geom_bar(stat = "identity", position=position_dodge(width = 1.5), alpha=0.6)+ theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_y_continuous(name="Abundance (thousands)", limits = c(0,400), breaks = seq(0,400,50)) + scale_x_continuous(name="Length(cm)",breaks = seq(1, 100, 3)) + theme(legend.position = c(0.9,0.8))+ theme(text=element_text(size=14)) + theme(legend.text = element_text(size=14)) + scale_fill_discrete(name="Year")+ geom_line(aes(Flen, Longterm), linetype=2,size=1) +ylim(0, 450) + ggtitle ("NMFS Spring")
#lf_nmfsspr

#ggsave(here("figures/NMFSSpring_Length_Frequency.png"))

#New Code:
#Generates its own longterm average. Just needs a new line added to LF_byYear_DFO.csv/
#Longterm mean based on average number per length over the previous 10 years. 
LF <- read.csv(here('data/LF_ByYear_NMFSspring.csv'))
terminalYear<-max(LF$Year)
TenYrAveYears<-subset(LF, Year<(terminalYear-1)&Year>(terminalYear-1-10)) #If survey is missing, it will only be a nine year average.
TenYrAveYears<-melt(TenYrAveYears, id.vars=c('Year'))
TenYrAveYears$variable<-as.numeric(with(TenYrAveYears, substring(variable,2)))#Removes le X.
TenYrAve<-aggregate(value~variable, FUN="mean", TenYrAveYears)
TenYrAve$Year<-paste(terminalYear-1-11, terminalYear-2, sep="-")

RecentTwoYears<-subset(LF, Year>(terminalYear-2)) #IN A NORMAL YEAR (NO MISSING SURVEY)
RecentTwoYears<-melt(RecentTwoYears, id.vars=c('Year'))
RecentTwoYears$variable<-as.numeric(with(RecentTwoYears, substring(variable,2)))

lf_nmfsspring<-ggplot(RecentTwoYears, aes(variable, value/1000, group=Year, fill=as.factor(Year))) + geom_bar(stat = "identity", position=position_dodge(), alpha=0.6)+ theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_y_continuous(name="Abundance (thousands)") + scale_x_continuous(name="Length(cm)",breaks = seq(0, 118, 5)) + theme(legend.position = c(0.8,0.8))+ theme(text=element_text(size=14)) + theme(legend.text = element_text(size=14))+ scale_fill_discrete(name="Year")+ geom_line(data=TenYrAve, aes(variable, value/1000, col=Year),linetype=2,size=1) + ggtitle("NMFS Spring")+ guides(col="none", fill=guide_legend(title="Year"))+scale_fill_viridis_d()+scale_colour_viridis_d()
lf_nmfsspring

ggsave(here("figures/NMFSSpring_Length_Frequency.png"), width=7.5, height=4.1, units="in")
