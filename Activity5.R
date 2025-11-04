# Isabel Drexler Booth
# Homework 5
#SETTING UP DATA AND VARIABLES
library(lubridate)
library(tidyverse)
#read in on PC
#datH <- read.csv("Z:\\idrexlerbooth\\data\\hw5_data\\stream_flow_data.csv",
#                 na.strings = c("Eqp"))
#datP <- read.csv("Z:\\idrexlerbooth\\data\\hw5_data\\2049867.csv")   

#read in on Mac
datH <- read.csv("/Volumes/CLASSspace/GEOG331_F25/idrexlerbooth/data/hw5_data/stream_flow_data.csv",
                na.strings = c("Eqp"))
datP <- read.csv("/Volumes/CLASSspace/GEOG331_F25/idrexlerbooth/data/hw5_data/2049867.csv")
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365)) 
#setting up aveF
#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")


#Question 5
dat2017 <- subset(datD, year == 2017)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     lwd = 2,
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA) #no border
#2017 line
lines(dat2017$doy, dat2017$discharge, col = "tomato3", lwd = 2)
#month ticks to make the x axis look pretty and be spaced out as required
month_ticks <-yday(ymd(paste(2017, 1:12, 1, sep = "-")))
month_ticks <- c(month_ticks, yday(ymd(paste(2018, 1:2, 1, sep = "-"))) + 365)

axis(side = 1, at = month_ticks, labels = c(month.abb, "Jan", "Feb"), las = 2) #tick intervals
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "2017 line"), #legend items
       lwd=c(2,NA, 2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "tomato3"),#colors
       pch=c(NA,15, NA),#symbols
       bty="n")#no legend border


#Question 7
#advice from the email
complete_day <- aggregate(datP$doy, by= list(datP$doy, datP$year), FUN = "length")
colnames(complete_day) <- c("doy","year", "length")
complete_day <- subset(complete_day, length == 24)

#connecting full data days to datD
#added first & datD$year
datD$complete_day <- ifelse(datD$doy %in% complete_day$doy & datD$year %in% complete_day$year, TRUE, FALSE)

#calculate a decimal year, but account for leap year
complete_day$decYear <- ifelse(leap_year(complete_day$year),complete_day$year + (complete_day$doy/366),
                        complete_day$year + (complete_day$doy/365))
par(mai=c(1,1,1,1))
#make plot
plot(datD$decYear,datD$discharge, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)#no axes
points(datD$decYear[datD$complete_day], datD$discharge[datD$complete_day], 
       col="blue", pch=16)
legend("topright", c("All measurements", "Day with 24 hours of precipitation"), 
       col=c("black", "blue"), 
       pch=c(NA, 16), lwd=c(1, NA), bty="n")


#Question 8 -- Making a Hydrograph
###### FIRST- choose a date range to look at (2 days). Check that both days either 
  #### have full precipitation measurements, or zero precipitation on days where this is missing
  #### subsest discharge and precipitation within day range of interest
hydroD <- datD[datD$doy >= 7 & datD$doy < 9 & datD$year == 2009,]
hydroP <- datP[datP$doy >= 7 & datP$doy < 9 & datP$year == 2009,]
###check that min is not by zero (this is very bad) (this is kept to explain how I chose my date range)
#min(hydroD$discharge)
###scale precipitation data to account for y min htat is not zero
#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl
###make the plot
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}
legend("topright", c("Discharge","Precipitation"), #legend items
       lwd=c(2,NA, 2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border


#Question 9 -- making violin plots
#Using nested ifs to make a column specifying the season
datD$season <- ifelse((datD$doy >= 355) | (datD$doy < 79), "Winter",
                      ifelse((datD$doy >= 79) & (datD$doy < 172), "Spring",
                            ifelse((datD$doy >= 172) & (datD$doy < 265), "Summer",
                                  ifelse((datD$doy >= 265) & (datD$doy < 355), "Fall", "NA"))))
#subseting data to only look at data from 2016
str_flow2016 <- subset(datD, year == 2016)
#factoring and making the plot
seasonPlot <- as.factor(str_flow2016$season)
ggplot(data = str_flow2016, aes(seasonPlot, discharge)) + 
  geom_violin() + 
  labs(x = "Season", y = expression(paste("Discharge ft"^"3 ","sec"^"-1")), title = "Discharge in Each Season of 2016")
#subsetting data to only look at data from 2017
str_flow2017 <- subset(datD, year == 2017)
#factoring and making the plot
seasonPlot <- as.factor(str_flow2017$season)
ggplot(data = str_flow2017, aes(seasonPlot, discharge)) + 
  geom_violin() + 
  labs(x = "Season", y = expression(paste("Discharge ft"^"3 ","sec"^"-1")), title = "Discharge in Each Season of 2017")


