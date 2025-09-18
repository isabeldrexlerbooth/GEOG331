#new script for F25 HW2
#pc file path
datW <- read.csv("Z:\\idrexlerbooth\\data\\noaa_data\\2011124.csv")
str(datW)
#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

words <- c("red", "yellow", "green", "blue", "purple")
numbers <- c(1, 2, 3.5, 5, 6.43)
num_int <- as.integer(1, 2, 3, 4, 5)
list <- c("high", "low", "high", "medium", "low")
list_factor <- factor(list)

unique(datW$NAME)
#look at the mean maximum temperature for Aberdeen
#with na.rm argument set to true to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#changing format as indicated by activity instructions
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$NAME <- factor(datW$NAME)
datW$siteN <- as.numeric(datW$NAME)

datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

par(mfrow=c(2,2))
#make a histogram for the first site in our levels, Aberdeen
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(x = datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#Site 2
hist(x = datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="darkmagenta",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "lightpink1",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "lightpink1", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "lightpink1", 
       lty = 3,
       lwd = 3)

#Site 3
hist(x = datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="darkolivegreen",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "darkolivegreen1",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "darkolivegreen1", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "darkolivegreen1", 
       lty = 3,
       lwd = 3)
       
#Site 4
hist(x = datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="slategray1",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "navy",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "navy", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "navy", 
       lty = 3,
       lwd = 3)
#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
#note I've named the histogram so I can reference it later
h1 <- hist(datW$TAVE[datW$siteN == 1],
        freq=FALSE, 
        main = paste(levels(datW$NAME)[1]),
        xlab = "Average daily temperature (degrees C)", 
        ylab="Relative frequency",
        col="grey50",
        border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

 y.plot <-  dnorm(seq(-10,30, length.out = 100),
             mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
             sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
 y.scaled <- (max(h1$density)/max(y.plot)) * y.plot
   
#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
1 - pnorm(18.51026, 
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
          sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))

1 - pnorm(qnorm(0.95,
                mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)), 
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
          sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))

par(mfrow=c(2,2))
annualPrcp <- aggregate(PRCP~NAME + year, data=datW, FUN=sum, na.rm=TRUE)
site_meanPrcp <- aggregate(PRCP~NAME + year, data=annualPrcp, FUN=mean, na.rm=TRUE)
site_meanPrcp
#name of the site the histogram will be based on
aberdeen <- levels(factor(annualPrcp$NAME))[1]
#annual totals
aberdeen_annual <- subset(annualPrcp, NAME == siteName)

hist(aberdeen_annual$PRCP,
     freq =FALSE,
     main=paste("Annual Precipitation -", annualPrcp$NAME[1]),
     xlab = "Annual Precipitation, mm",
     ylab = "Relative Frequency",
     col = "slategrey",
     border = "white")

#do it again :P
#name of the site the histogram will be based on
livermore <- levels(factor(annualPrcp$NAME))
#annual totals
livermore_annual <-subset(annualPrcp, (annualPrcp$NAME)[2])

hist(livermore_annual$PRCP,
     freq =FALSE,
     main=paste("Annual Precipitation -", annualPrcp$NAME[2]),
     xlab = "Annual Precipitation, mm",
     ylab = "Relative Frequency",
     col = "moccasin",
     border = "white")

#do it again :P
#name of the site the histogram will be based on
mes <- levels(annualPrcp$NAME)[3]
mes_annual <- subset(annualPrcp, NAME == mes)
hist(mes_annual$PRCP,
     freq =FALSE,
     main=paste("Annual Precipitation -", mes),
     xlab = "Annual Precipitation, mm",
     ylab = "Relative Frequency",
     col = "skyblue1",
     border = "white")

#do it again :P
#name of the site the histogram will be based on
mormon <- levels(annualPrcp$NAME)[4]
mormon_annual <- subset(annualPrcp, NAME == mormon)
hist(mormon_annual$PRCP,
     freq =FALSE,
     main=paste("Annual Precipitation -", mormon),
     xlab = "Annual Precipitation, mm",
     ylab = "Relative Frequency",
     col = "lightsteelblue1",
     border = "white")

#do it again :P
#name of the site the histogram will be based on
morris <- levels(annualPrcp$NAME)[5]
morris_annual <- subset(annualPrcp, NAME == morris)
hist(morris_annual$PRCP,
     freq =FALSE,
     main=paste("Annual Precipitation -", morris),
     xlab = "Annual Precipitation, mm",
     ylab = "Relative Frequency",
     col = "royalblue4",
     border = "white")


par(mfrow=c(2,5))
