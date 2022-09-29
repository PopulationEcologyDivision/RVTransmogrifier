#############################
# Creating CAA plots in R
# July 16, 2013
# Author: Christine Hansen
#############################

#Load libraries: 
require(ggplot2)
require(shapefiles)
require(grid)
require(geosphere)
require(grDevices)
require(gridExtra)
require(here)

#Load map--------------

#Depth Contours, boundaries and strata: 
#contours
DC12 <- read.shp(here("data/USGSContour100_200Geo.shp")) #read shape file 
DC12 <- convert.to.simple(DC12) #simplify the shp file to a simple data.frame

strata5Z <- rbind(c(42.33333,67.6666667), c(41.833333,67.666667), c(41.8333333,65.666667), 
                  c(42.000000,65.666667), c(42.333333,66.000000), c(42.3333333,67.666667))
strata5Z <- as.data.frame(strata5Z); strata5Z$Id <- rep('j')
strata5Z <- rbind(strata5Z, c(41.833333,67.666667,'m'), c(41.166667,67.666667,'m'),
                  c(41.166667,67.166667,'m'),c(41.000000,67.166667,'m'),c(41.000000,67.000000,'m'), 
                  c(40.5, 67.0, 'm'),c(40.0, 67.0, 'm'), #these are added points
                  c(39.833333,67.000000,'m'),c(39.833333,66.833333,'m'), c(39.666667,66.833333,'m'), 
                  c(39.666667,66.666667,'m'),c(39.000000,66.666667,'m'),c(39.000000,65.666667,'m'),
                  c(40.0,65.666667, 'm'),c(40.5,65.666667, 'm'), #these are added points
                  c(41.833333,65.666667,'m'),c(41.833333,67.666667,'m'))
names(strata5Z) <- c("Y","X","Id") #column name
strata5Z$X <- as.numeric(strata5Z$X); strata5Z$Y <- as.numeric(strata5Z$Y)
strata5Z$X <- strata5Z$X *-1

#boundaries
border <- read.shp(here("data/USCanBorder.shp"))
border <- convert.to.simple(border)
border <- subset(border, X<(-65) & X> -70 & Y<43 & Y>40)
GBorder <- subset(border, Y>40.5); LBorder <- subset(border, Y<40.5)
border <- rbind(GBorder,c(58,-67.45191,42.5), c(58,-65.7413,40.5), LBorder)

#Combine for base plot
at <- seq(68,65,-1)
at2 <- seq(41,42,1)
L <- parse(text=paste(at, "*degree ", sep=""))
L2 <- parse(text=paste(at2, "*degree ", sep=""))

g <- ggplot(DC12) + geom_path(aes(x=X,y=Y,group=Id)) + geom_path(data=strata5Z, aes(x=X,y=Y,group=Id))+
  geom_path(data=border, aes(x=X,y=Y,group=Id),linetype=2) + coord_map() + ylab("") + xlab("") + 
  theme_bw() + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  scale_x_continuous(breaks=c(-68,-67,-66,-65), labels=L, limits=c(-68,-65)) + 
  scale_y_continuous(breaks=at2, labels=L2, limits=c(40.5,42.5)) 

#DFO Distribution-------------------------

### Latest year
a2022 <- read.csv(here("data/DFO Dist/Cod2022.csv")) #most recent year for 1yr plot
a2022 <- a2022[,c(2,3,6:24)] # remove unnecessary columns 
colnames(a2022) <-c("Slat", "Slong", "Age0", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6",
                    "Age7", "Age8", "Age9", "Age10", "Age11", "Age12", "Age13", "Age14", "Age15", 
                    "Age16", "Unknown", "Total")
a2022$Age3Pl <- with(a2022, Age3+Age4+Age5+Age6+Age7+Age8+Age9+Age10+Age11+Age12+Age13+Age14+Age15+Age16)
a2022$Lat <- as.numeric(substr(a2022$Slat,1,2)) + as.numeric(substr(a2022$Slat,3,6))/60
a2022$Slong <- a2022$Slong*-1
a2022$Long <- (as.numeric(substr(a2022$Slong,1,2)) + as.numeric(substr(a2022$Slong,3,6))/60)*-1
a2022$Group <- ifelse(a2022$Age3Pl==0,0,ifelse(a2022$Age3Pl<=0.001,1,ifelse(a2022$Age3Pl<=20,2,
             ifelse(a2022$Age3Pl<=100,3, ifelse(a2022$Age3Pl<=200,4,5)))))
table(a2022$Group, useNA="ifany") #to check

Plot_1yr <- g + geom_point(data=a2022, aes(x=Long, y=Lat, size=as.factor(Group),shape=as.factor(Group)),
                colour="red")+ coord_map() + geom_text(label="Age 3+", aes(x=-66.7,y=40.5), size=7) +
                scale_size_manual(values=c(2,2,3,4,5,7),name="Number/Tow", 
                breaks=c("0","1","2","3","4","5"), labels=c("0","0.001","20","100","200",">200")) +
                scale_shape_manual(values=c(3,16,16,16,16,16),name="Number/Tow", 
                breaks=c("0","1","2","3","4","5"),labels=c("0","0.001","20","100","200",">200")) + 
                theme(legend.position=c(0.85,0.2), legend.direction="vertical", 
                legend.key=element_blank()) 
Plot_1yr

### 10 year average
a2021 <- read.csv(here("data/DFO Dist/Cod2021.csv")); a2021$V3 <- rep(2021)
a2020 <- read.csv(here("data/DFO Dist/Cod2020.csv")); a2020$V3 <- rep(2020)
a2019 <- read.csv(here("data/DFO Dist/Cod2019.csv")); a2019$V3 <- rep(2019)
a2018 <- read.csv(here("data/DFO Dist/Cod2018.csv")); a2018$V3 <- rep(2018)
a2017 <- read.csv(here("data/DFO Dist/Cod2017.csv")); a2017$V3 <- rep(2017)
a2016 <- read.csv(here("data/DFO Dist/Cod2016.csv")); a2016$V3 <- rep(2016)
a2015 <- read.csv(here("data/DFO Dist/Cod2015.csv")); a2015$V3 <- rep(2015)
a2014 <- read.csv(here("data/DFO Dist/Cod2014.csv")); a2014$V3 <- rep(2014)
a2013 <- read.csv(here("data/DFO Dist/Cod2013.csv")); a2013$V3 <- rep(2013)
a2012 <- read.csv(here("data/DFO Dist/Cod2012.csv")); a2012$V3 <- rep(2012)

#check that all of these files have the same # of columns, or rbind will not work
#in this case, a2004 has one extra column (the 3+ age group), remove and then continue with rbind
#a2004 <- a2004[,c(1:24,26)]
a10yr <- rbind(a2021,a2020,a2019,a2018,a2017,a2016,a2015,a2014,a2013,a2012)
rm(a2021, a2020,a2019,a2018,a2017,a2016,a2015,a2014,a2013,a2012)
a10yr <- a10yr[,c(2,3,6:25)] # remove unnecessary columns 
colnames(a10yr) <-c("Slat", "Slong", "Age0", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6",
                    "Age7", "Age8", "Age9", "Age10", "Age11", "Age12", "Age13", "Age14", "Age15", 
                    "Age16", "Unknown", "Total", "Year")
a10yr$POS <- paste(a10yr$Slat, a10yr$Slong, sep=", ") #create a character vector that represents the lat/long
  #position of all densities; this allows us to average across years by position
a10yr$Age3Pl <- with(a10yr, Age3+Age4+Age5+Age6+Age7+Age8+Age9+Age10+Age11+Age12+Age13+Age14+Age15+Age16)
avg <- as.data.frame(tapply(a10yr$Age3Pl, a10yr$POS, mean))
names(avg) <- "Mean"; avg$POS <- rownames(avg)
dat1 <- data.frame(do.call(rbind, strsplit(as.vector(avg$POS), split = ", ")))
avg <- cbind(avg,dat1); row.names(avg)<-seq(1,length(avg$POS),1)
avg <-avg[,c(1,3,4)]; colnames(avg) <- c("Mean", "Slat", "Slong")
avg$Lat <- as.numeric(substr(avg$Slat,1,2)) + as.numeric(substr(avg$Slat,3,6))/60
avg$Long <- (as.numeric(substr(avg$Slong,1,2)) + as.numeric(substr(avg$Slong,3,6))/60)*-1
avg$Group <- ifelse(avg$Mean==0,0,ifelse(avg$Mean<=0.001,1,ifelse(avg$Mean<=20,2,
             ifelse(avg$Mean<=100,3, ifelse(avg$Mean<=200,4,5)))))
avg$m2 <- ifelse(avg$Mean<=200, avg$Mean, 201)
avg <- na.omit(avg)
avg_neg <- avg[,c(4,5)]
avg_neg$MeanREV <- avg$Mean* -1

avg_neg$MeanREV <- ifelse(avg_neg$MeanREV < (-200), -300, avg_neg$MeanREV)

cols <- c("#FFFFCC", "#66FFFF", "#3399CC", "#0066CC", "#003399")
Plot_10yr <- ggplot(avg_neg) + stat_summary_2d(aes(x=Long, y=Lat, z=MeanREV), fun=mean) +
             scale_fill_gradientn(name="Avg Number/Tow",colours=rev(cols),guide="colourbar",
             breaks=c(0,-20,-100,-200, -300), labels=c(0,20,100,200,'200+')) + 
             coord_map() + ylab("") + xlab("") + theme_bw() +
             theme(legend.position=c(0.85,0.2),legend.direction="vertical", 
             panel.grid.major=element_blank(),panel.grid.minor=element_blank()) + 
             scale_x_continuous(breaks=c(-68,-67,-66,-65), labels=L, limits=c(-68,-65)) + 
             scale_y_continuous(breaks=at2, labels=L2, limits=c(40.5,42.5)) + 
             geom_path(data=DC12,aes(x=X,y=Y,group=Id)) + 
             geom_path(data=strata5Z, aes(x=X,y=Y,group=Id)) + 
             geom_path(data=border, aes(x=X,y=Y,group=Id), linetype=2) + 
             geom_text(label="Age 3+", aes(x=-66.7,y=40.5), size=7)

Plot_10yr

######Both plots

Layout <- grid.layout(nrow = 1, ncol = 2)
grid.show.layout(Layout)

vplayout <- function(...) {
  grid.newpage()
  pushViewport(viewport(layout = Layout))
} 

subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)

mmplot <- function(a, b) {
  vplayout()
  print(a, vp = subplot(1, 1))
  print(b, vp = subplot(1, 2))
} 

#jpeg(here("figures/DFOSurvey_Distribution.jpeg"), width=1200, height=497) 
#mmplot(Plot_10yr,Plot_1yr)
#dev.off()

#####Cutting and Pasting method to obtain correct legend

#No legend
Plot_10yrNL <- ggplot(avg_neg) + stat_summary_2d(aes(x=Long, y=Lat, z=MeanREV), fun=mean) +
  scale_fill_gradientn(name="Avg Number/Tow",colours=rev(cols),guide="colourbar",
                       breaks=c(0,-20,-100,-200, -300), labels=c(0,20,100,200,'200+')) + 
  coord_map() + ylab("") + xlab("") + theme_bw() +
  theme(legend.position='none',
        panel.grid.major=element_blank(),panel.grid.minor=element_blank()) + 
  scale_x_continuous(breaks=c(-68,-67,-66,-65), labels=L, limits=c(-68,-65)) + 
  scale_y_continuous(breaks=at2, labels=L2, limits=c(40.5,42.5)) + 
  geom_path(data=DC12,aes(x=X,y=Y,group=Id)) + 
  geom_path(data=strata5Z, aes(x=X,y=Y,group=Id)) + 
  geom_path(data=border, aes(x=X,y=Y,group=Id), linetype=2) + 
  geom_text(label="Age 3+", aes(x=-66.7,y=40.5), size=7)

min(avg_neg$MeanREV)
max(avg_neg$MeanREV)
#Create plot with correct legend
Goodlegend <- ggplot(avg_neg) + stat_summary_2d(aes(x=Long, y=Lat, z=MeanREV), fun=mean) +
  scale_fill_gradientn(name="Avg Number/Tow",colours=rev(cols),guide="legend",
                       breaks=c(0,-20,-100,-200, -300), labels=c(0,20,100,200,'200+')) + 
  coord_map() + ylab("") + xlab("") + theme_bw() +
  theme(legend.direction="vertical", 
        panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

#Extract legend
require(gridExtra)
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(Goodlegend) 

#Add legend to no-legend plot
vpleg <- viewport(width = 0.1, height = 0.1, x=0.78,y=0.22)
layout(matrix(c(1,2),1,2,byrow=T),widths=c(3,1))
plot(Plot_10yrNL)
pushViewport(vpleg)
grid.draw(legend)

#### Both plots
vpleg <- viewport(width = 0.1, height = 0.1, x=0.42,y=0.24)
layout(matrix(c(1,2),1,2,byrow=T),widths=c(3,1))
jpeg(here("figures/DFOSurvey_Distribution.png"), width=1200, height=497) 
mmplot(Plot_10yrNL, Plot_1yr)
pushViewport(vpleg)
grid.draw(legend)
dev.off()




#NMFS Spring Distribution -----------------

### Latest year
a2022 <- read.csv(here("data/NMFS Spr Dist/cod2022.csv")) #most recent year for 3yr plot
head(a2022)
a2022$Age3Pl <- with(a2022, X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16.)
a2022$Lat <- as.numeric(substr(a2022$SLAT,1,2)) + as.numeric(substr(a2021$SLAT,3,6))/60
a2022$Long <- (as.numeric(substr(a2022$SLONG,1,2)) + as.numeric(substr(a2021$SLONG,3,6))/60)*-1

a2022$Group <- ifelse(a2022$Age3Pl==0,0,ifelse(a2022$Age3Pl<=0.001,1,ifelse(a2022$Age3Pl<=20,2,
                                                                            ifelse(a2022$Age3Pl<=100,3, ifelse(a2022$Age3Pl<=200,4,5)))))
head(a2022)
table(a2022$Group, useNA="ifany") #to check

Plot_3yr <- g + geom_point(data=a2022, aes(x=Long, y=Lat, size=as.factor(Group),shape=as.factor(Group)),
                           colour="red")+ coord_map() + geom_text(label="Age 3+", aes(x=-66.7,y=40.5), size=7) +
  scale_size_manual(values=c(2,2,3,4,5,7),name="Number/Tow", 
                    breaks=c("0","1","2","3","4","5"), labels=c("0","0.001","20","100","200",">200")) +
  scale_shape_manual(values=c(3,16,16,16,16,16),name="Number/Tow", 
                     breaks=c("0","1","2","3","4","5"),labels=c("0","0.001","20","100","200",">200")) + 
  theme(legend.position=c(0.85,0.2), legend.direction="vertical", 
        legend.key=element_blank()) 
Plot_3yr

### 10 year average (really 9 year because 2020 is missing)
#Set working directory
setwd(here("data/NMFS Spr Dist"))
a2021 <- read.csv(here("data/NMFS Spr Dist/cod2021.csv")); a2021$V3 <- rep(2021)
a2019 <- read.csv(here("data/NMFS Spr Dist/cod2019.csv")); a2019$V3 <- rep(2019)
a2018 <- read.csv(here("data/NMFS Spr Dist/cod2018.csv")); a2018$V3 <- rep(2018)
a2017 <- read.csv(here("data/NMFS Spr Dist/cod2017.csv")); a2017$V3 <- rep(2017)
a2016 <- read.csv(here("data/NMFS Spr Dist/cod2016.csv")); a2016$V3 <- rep(2016)
a2015 <- read.csv(here("data/NMFS Spr Dist/cod2015.csv")); a2015$V3 <- rep(2015)
a2014 <- read.csv(here("data/NMFS Spr Dist/cod2014.csv")); a2014$V3 <- rep(2014)
a2013 <- read.csv(here("data/NMFS Spr Dist/cod2013.csv")); a2013$V3 <- rep(2013)
a2012 <- read.csv(here("data/NMFS Spr Dist/cod2012.csv")); a2012$V3 <- rep(2012)

#check that all of these files have the same # of columns, or rbind will not work
a10yr <- rbind(a2021, a2019,a2018,a2017,a2016,a2015,a2014,a2013,a2012)
rm(a2021,a2019,a2018,a2017,a2016,a2015,a2014,a2013,a2012)
a10yr <- a10yr[,c(2,3,6:25)] # remove unnecessary columns 
colnames(a10yr) <-c("Slat", "Slong", "Age0", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6",
                    "Age7", "Age8", "Age9", "Age10", "Age11", "Age12", "Age13", "Age14", "Age15", 
                    "Age16", "Unknown", "Total", "Year")
a10yr$POS <- paste(a10yr$Slat, a10yr$Slong, sep=", ") #create a character vector that represents the lat/long
#position of all densities; this allows us to average across years by position
a10yr$Age3Pl <- with(a10yr, Age3+Age4+Age5+Age6+Age7+Age8+Age9+Age10+Age11+Age12+Age13+Age14+Age15+Age16)
avg <- as.data.frame(tapply(a10yr$Age3Pl, a10yr$POS, mean))
names(avg) <- "Mean"; avg$POS <- rownames(avg)
dat1 <- data.frame(do.call(rbind, strsplit(as.vector(avg$POS), split = ", ")))
avg <- cbind(avg,dat1); row.names(avg)<-seq(1,length(avg$POS),1)
avg <-avg[,c(1,3,4)]; colnames(avg) <- c("Mean", "Slat", "Slong")
avg$Lat <- as.numeric(substr(avg$Slat,1,2)) + as.numeric(substr(avg$Slat,3,6))/60
avg$Long <- (as.numeric(substr(avg$Slong,1,2)) + as.numeric(substr(avg$Slong,3,6))/60)*-1
avg$Group <- ifelse(avg$Mean==0,0,ifelse(avg$Mean<=1,1,ifelse(avg$Mean<=2,2,ifelse(avg$Mean<=10,3,ifelse(avg$Mean<=20,4,ifelse(avg$Mean<=50,5,
                                                                                                                               ifelse(avg$Mean<=100,6, ifelse(avg$Mean<=200,7,8))))))))
avg$m2 <- ifelse(avg$Mean<=200, avg$Mean, 201)
avg <- na.omit(avg)
avg_neg <- avg[,c(4,5)]
avg_neg$MeanREV <- avg$Mean* -1
avg_neg$MeanREV <- ifelse(avg_neg$MeanREV < (-200), -300, avg_neg$MeanREV)

cols <- c("#FFFFCC", "#66FFFF", "#3399CC", "#0066CC", "#003399")
Plot_10yr <- ggplot(avg_neg) + stat_summary_2d(aes(x=Long, y=Lat, z=MeanREV), fun=mean) +
  scale_fill_gradientn(name="Avg Number/Tow",colours=rev(cols),guide="colourbar",
                       breaks=c(0,-1,-2,-10,-20,-50,-100,-200, -300), labels=c(0,1,2,10,20,'>20',100,200,'200+')) + 
  coord_map() + ylab("") + xlab("") + theme_bw() +
  theme(legend.position=c(0.85,0.2),legend.direction="vertical", 
        panel.grid.major=element_blank(),panel.grid.minor=element_blank()) + 
  scale_x_continuous(breaks=c(-68,-67,-66,-65), labels=L, limits=c(-68,-65)) + 
  scale_y_continuous(breaks=at2, labels=L2, limits=c(40.5,42.5)) + 
  geom_path(data=DC12,aes(x=X,y=Y,group=Id)) + 
  geom_path(data=strata5Z, aes(x=X,y=Y,group=Id)) + 
  geom_path(data=border, aes(x=X,y=Y,group=Id), linetype=2) + 
  geom_text(label="Age 3+", aes(x=-66.7,y=40.5), size=7)

Plot_10yr

######Both plots

Layout <- grid.layout(nrow = 1, ncol = 2)
grid.show.layout(Layout)

vplayout <- function(...) {
  grid.newpage()
  pushViewport(viewport(layout = Layout))
} 

subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)

mmplot <- function(a, b) {
  vplayout()
  print(a, vp = subplot(1, 1))
  print(b, vp = subplot(1, 2))
} 

jpeg(here("figures/NMFSSpringSurvey_Distribution.jpeg"), width=1200, height=497) 
mmplot(Plot_10yr,Plot_3yr)
dev.off()

#####Cutting and Pasting method to obtain correct legend

#No legend
Plot_10yrNL <- ggplot(avg_neg) + stat_summary_2d(aes(x=Long, y=Lat, z=MeanREV), fun=mean) +
  scale_fill_gradientn(name="Avg Number/Tow",colours=rev(cols),guide="colourbar",
                       breaks=c(0,-20,-100,-200, -300), labels=c(0,20,100,200,'200+')) + 
  coord_map() + ylab("") + xlab("") + theme_bw() +
  theme(legend.position='none',
        panel.grid.major=element_blank(),panel.grid.minor=element_blank()) + 
  scale_x_continuous(breaks=c(-68,-67,-66,-65), labels=L, limits=c(-68,-65)) + 
  scale_y_continuous(breaks=at2, labels=L2, limits=c(40.5,42.5)) + 
  geom_path(data=DC12,aes(x=X,y=Y,group=Id)) + 
  geom_path(data=strata5Z, aes(x=X,y=Y,group=Id)) + 
  geom_path(data=border, aes(x=X,y=Y,group=Id), linetype=2) + 
  geom_text(label="Age 3+", aes(x=-66.7,y=40.5), size=7)

min(avg_neg$MeanREV)
max(avg_neg$MeanREV)
#Create plot with correct legend
Goodlegend <- ggplot(avg_neg) + stat_summary_2d(aes(x=Long, y=Lat, z=MeanREV), fun=mean) +
  scale_fill_gradientn(name="Avg Number/Tow",colours=rev(cols),guide="legend",
                       breaks=c(0,-20,-100,-200, -300), labels=c(0,20,100,200,'200+')) + 
  coord_map() + ylab("") + xlab("") + theme_bw() +
  theme(legend.direction="vertical", 
        panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

#Extract legend
require(gridExtra)
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(Goodlegend) 

#Add legend to no-legend plot
vpleg <- viewport(width = 0.1, height = 0.1, x=0.78,y=0.22)
layout(matrix(c(1,2),1,2,byrow=T),widths=c(3,1))
plot(Plot_10yrNL)
pushViewport(vpleg)
grid.draw(legend)

#### Both plots
vpleg <- viewport(width = 0.1, height = 0.1, x=0.42,y=0.24)
layout(matrix(c(1,2),1,2,byrow=T),widths=c(3,1))
jpeg(here("figures/NMFSSpringSurvey_Distribution.png"), width=1200, height=497) 
mmplot(Plot_10yrNL, Plot_3yr)
pushViewport(vpleg)
grid.draw(legend)
dev.off()
vpleg




#NMFS Fall Distribution -----------------

### Latest year
a2021 <- read.csv(here("data/NMFS Fall Dist/cod2021.csv")) #most recent year for 3yr plot
head(a2021)
#a2021 <- a2021[,c(1,4)] # remove unnecessary columns, pick columns you want to keep 
a2021$Age3Pl <- with(a2021, X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16.)
a2021$Lat <- as.numeric(substr(a2021$SLAT,1,2)) + as.numeric(substr(a2021$SLAT,3,6))/60
a2021$Long <- (as.numeric(substr(a2021$SLONG,1,2)) + as.numeric(substr(a2021$SLONG,3,6))/60)*-1

a2021$Group <- ifelse(a2021$Age3Pl==0,0,ifelse(a2021$Age3Pl<=0.001,1,ifelse(a2021$Age3Pl<=20,2,
                     ifelse(a2021$Age3Pl<=100,3, ifelse(a2021$Age3Pl<=200,4,5)))))
head(a2021)
table(a2021$Group, useNA="ifany") #to check

Plot_3yr <- g + geom_point(data=a2021, aes(x=Long, y=Lat, size=as.factor(Group),shape=as.factor(Group)),
                           colour="red")+ coord_map() + geom_text(label="Age 3+", aes(x=-66.7,y=40.5), size=7) +
  scale_size_manual(values=c(2,2,3,4,5,7),name="Number/Tow", 
                    breaks=c("0","1","2","3","4","5"), labels=c("0","0.001","20","100","200",">200")) +
  scale_shape_manual(values=c(3,16,16,16,16,16),name="Number/Tow", 
                     breaks=c("0","1","2","3","4","5"),labels=c("0","0.001","20","100","200",">200")) + 
  theme(legend.position=c(0.85,0.2), legend.direction="vertical", 
        legend.key=element_blank()) 

Plot_3yr

### 10 year average (really 9 year because 2020 is missing)
a2019 <- read.csv(here("data/NMFS Fall Dist/cod2019.csv")); a2019$V3 <- rep(2019)
a2018 <- read.csv(here("data/NMFS Fall Dist/cod2018.csv")); a2018$V3 <- rep(2018)
a2017 <- read.csv(here("data/NMFS Fall Dist/cod2017.csv")); a2017$V3 <- rep(2017)
a2016 <- read.csv(here("data/NMFS Fall Dist/cod2016.csv")); a2016$V3 <- rep(2016)
a2015 <- read.csv(here("data/NMFS Fall Dist/cod2015.csv")); a2015$V3 <- rep(2015)
a2014 <- read.csv(here("data/NMFS Fall Dist/cod2014.csv")); a2014$V3 <- rep(2014)
a2013 <- read.csv(here("data/NMFS Fall Dist/cod2013.csv")); a2013$V3 <- rep(2013)
a2012 <- read.csv(here("data/NMFS Fall Dist/cod2012.csv")); a2012$V3 <- rep(2012)
a2011 <- read.csv(here("data/NMFS Fall Dist/cod2011.csv")); a2012$V3 <- rep(2011)

#check that all of these files have the same # of columns, or rbind will not work
a10yr <- rbind(a2019,a2018,a2017,a2016,a2015,a2014,a2013,a2012)
rm(a2019,a2018,a2017,a2016,a2015,a2014,a2013,a2012)
a10yr <- a10yr[,c(2,3,6:25)] # remove unnecessary columns 
colnames(a10yr) <-c("Slat", "Slong", "Age0", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6",
                    "Age7", "Age8", "Age9", "Age10", "Age11", "Age12", "Age13", "Age14", "Age15", 
                    "Age16", "Unknown", "Total", "Year")
a10yr$POS <- paste(a10yr$Slat, a10yr$Slong, sep=", ") #create a character vector that represents the lat/long
#position of all densities; this allows us to average across years by position
a10yr$Age3Pl <- with(a10yr, Age3+Age4+Age5+Age6+Age7+Age8+Age9+Age10+Age11+Age12+Age13+Age14+Age15+Age16)
avg <- as.data.frame(tapply(a10yr$Age3Pl, a10yr$POS, mean))
names(avg) <- "Mean"; avg$POS <- rownames(avg)
dat1 <- data.frame(do.call(rbind, strsplit(as.vector(avg$POS), split = ", ")))
avg <- cbind(avg,dat1); row.names(avg)<-seq(1,length(avg$POS),1)
avg <-avg[,c(1,3,4)]; colnames(avg) <- c("Mean", "Slat", "Slong")
avg$Lat <- as.numeric(substr(avg$Slat,1,2)) + as.numeric(substr(avg$Slat,3,6))/60
avg$Long <- (as.numeric(substr(avg$Slong,1,2)) + as.numeric(substr(avg$Slong,3,6))/60)*-1
avg$Group <- ifelse(avg$Mean==0,0,ifelse(avg$Mean<=1,1,ifelse(avg$Mean<=2,2,ifelse(avg$Mean<=10,3,ifelse(avg$Mean<=20,4,ifelse(avg$Mean<=50,5,
                                                                                                                               ifelse(avg$Mean<=100,6, ifelse(avg$Mean<=200,7,8))))))))
avg$m2 <- ifelse(avg$Mean<=200, avg$Mean, 201)
avg <- na.omit(avg)
avg_neg <- avg[,c(4,5)]
avg_neg$MeanREV <- avg$Mean* -1
avg_neg$MeanREV <- ifelse(avg_neg$MeanREV < (-200), -300, avg_neg$MeanREV)

cols <- c("#FFFFCC", "#66FFFF", "#3399CC", "#0066CC", "#003399")
Plot_10yr <- ggplot(avg_neg) + stat_summary_2d(aes(x=Long, y=Lat, z=MeanREV), fun=mean) +
  scale_fill_gradientn(name="Avg Number/Tow",colours=rev(cols),guide="colourbar",
                       breaks=c(0,-1,-2,-10,-20,-50,-100,-200, -300), labels=c(0,1,2,10,20,'>20',100,200,'200+')) + 
  coord_map() + ylab("") + xlab("") + theme_bw() +
  theme(legend.position=c(0.85,0.2),legend.direction="vertical", 
        panel.grid.major=element_blank(),panel.grid.minor=element_blank()) + 
  scale_x_continuous(breaks=c(-68,-67,-66,-65), labels=L, limits=c(-68,-65)) + 
  scale_y_continuous(breaks=at2, labels=L2, limits=c(40.5,42.5)) + 
  geom_path(data=DC12,aes(x=X,y=Y,group=Id)) + 
  geom_path(data=strata5Z, aes(x=X,y=Y,group=Id)) + 
  geom_path(data=border, aes(x=X,y=Y,group=Id), linetype=2) + 
  geom_text(label="Age 3+", aes(x=-66.7,y=40.5), size=7)

Plot_10yr

######Both plots

Layout <- grid.layout(nrow = 1, ncol = 2)
grid.show.layout(Layout)

vplayout <- function(...) {
  grid.newpage()
  pushViewport(viewport(layout = Layout))
} 

subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)

mmplot <- function(a, b) {
  vplayout()
  print(a, vp = subplot(1, 1))
  print(b, vp = subplot(1, 2))
} 

jpeg(here("figures/NMFSFallSurvey_Distribution.jpeg"), width=1200, height=497) 
mmplot(Plot_10yr,Plot_3yr)
dev.off()

#####Cutting and Pasting method to obtain correct legend

#No legend
Plot_10yrNL <- ggplot(avg_neg) + stat_summary_2d(aes(x=Long, y=Lat, z=MeanREV), fun=mean) +
  scale_fill_gradientn(name="Avg Number/Tow",colours=rev(cols),guide="colourbar",
                       breaks=c(0,-20,-100,-200, -300), labels=c(0,20,100,200,'200+')) + 
  coord_map() + ylab("") + xlab("") + theme_bw() +
  theme(legend.position='none',
        panel.grid.major=element_blank(),panel.grid.minor=element_blank()) + 
  scale_x_continuous(breaks=c(-68,-67,-66,-65), labels=L, limits=c(-68,-65)) + 
  scale_y_continuous(breaks=at2, labels=L2, limits=c(40.5,42.5)) + 
  geom_path(data=DC12,aes(x=X,y=Y,group=Id)) + 
  geom_path(data=strata5Z, aes(x=X,y=Y,group=Id)) + 
  geom_path(data=border, aes(x=X,y=Y,group=Id), linetype=2) + 
  geom_text(label="Age 3+", aes(x=-66.7,y=40.5), size=7)

min(avg_neg$MeanREV)
max(avg_neg$MeanREV)
#Create plot with correct legend
Goodlegend <- ggplot(avg_neg) + stat_summary_2d(aes(x=Long, y=Lat, z=MeanREV), fun=mean) +
  scale_fill_gradientn(name="Avg Number/Tow",colours=rev(cols),guide="legend",
                       breaks=c(0,-1,-2,-10,-20,-50,-100,-200, -300), labels=c(0,1,2,10,20,'>20',100,200,'200+')) + 
  coord_map() + ylab("") + xlab("") + theme_bw() +
  theme(legend.direction="vertical", 
        panel.grid.major=element_blank(),panel.grid.minor=element_blank()) 

#Extract legend
require(gridExtra)
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(Goodlegend) 

#Add legend to no-legend plot
vpleg <- viewport(width = 0.1, height = 0.1, x=0.78,y=0.22)
layout(matrix(c(1,2),1,2,byrow=T),widths=c(3,1))
plot(Plot_10yrNL)
pushViewport(vpleg)
grid.draw(legend)

#### Both plots
vpleg <- viewport(width = 0.1, height = 0.1, x=0.42,y=0.24)
layout(matrix(c(1,2),1,2,byrow=T),widths=c(3,1))
jpeg(here("figures/NMFSFallSurvey_Distribution.png"), width=1200, height=497) 
mmplot(Plot_10yrNL, Plot_3yr)
pushViewport(vpleg)
grid.draw(legend)
dev.off()
vpleg
