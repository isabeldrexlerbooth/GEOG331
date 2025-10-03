#Activity 3
#this only needs to be done once per computer (not per document)
#install.packages(c("lubridate"))
#this, however, needs to run for every document
library(lubridate)

#creates assert function. First arg is the conditional being evaluated, second arg is the error message that will be printed if assertion is false.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
}
#read in the data file for the pc
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently (is not read as a character)
#datW <- read.csv("Z:\\idrexlerbooth\\data\\bewkes\\bewkes_weather.csv",
#                 na.strings=c("#N/A"), skip=3, header=FALSE)
#get sensor info from file
# this data table will contain all relevant units
#sensorInfo <-   read.csv("Z:\\idrexlerbooth\\data\\bewkes\\bewkes_weather.csv",
#                         na.strings=c("#N/A"), nrows=2)
#read the file for my mac
datW <- read.csv("/Volumes/class/GEOG331_F25/idrexlerbooth/data/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#make sensorinfo for mac
sensorInfo <-   read.csv("/Volumes/class/GEOG331_F25/idrexlerbooth/data/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)


#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)


#Question 5
#first, we make lightscale to use in the assert function
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#now we can test by checking that a specific value in lightscale is the same as if we had done the scaling manually for every point
assert(lightscale[1] == (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy[1], "assertion failed: index one of lightscale is not equal to the first lightning value scaled to match [recipitation")
#a second assertion to confirm that columns are the same length (all data is included from lightning)
assert(length(lightscale) == length(datW$lightning.acvitivy), "assertion failed: input vectors do not have the same number of elements")

#Question 6 -- NEEDS TO BE FINISHED
#we will first find what values need to be remvoved
#first, make a new column with wind speeds so that we don't accidentally mess up our og column
datW$wind.speedQ6 <- ifelse(datW$wind.speed < 0, NA, datW$wind.speed)

#we will use the quantile function to identify the cutoff for the bottow 0% and the cutoff for those over 100%
#for the ease of readability, both will get their own variables (although this is not necessary)
windBottom <- quantile(datW$wind.speedQ6)[1]
windTop <- quantile(datW$wind.speedQ6)[5]
#now we remove the values less than the bottom and greater than the top from the new column
datW$wind.speedQ6 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))
#assert that values did in fact get reassigned as na
assert(sum(is.na(datW$wind.speedQ6)) > 0, "assertion failed: values were not reassigned from wind speed column")
#now lets make a plot (I'm assuming like the ones from the top of this assignment?)
plot(datW$DD, datW$wind.speedQ6, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind speed (m.s)")
plot(datW$DD, datW$wind.speed, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind speed (m.s)")
par(mfrow=c(1,2))

#Question 7

#Question 8
mean <- function(statement){
  #calculates the mean of a dataset
  avg <- sum(!is.na(satement)/length(statement)
  }


#Question 9
#let all four plots show on the same page
par(mfrow=c(2,2))

#Plot soil temperature
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Temperature (˚C")
#Plot air temperature
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air Temperature (˚C)")
#Plot soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Moisture (meters cubed per meter cubed")

#Plot precipitation
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (mm)")

par(mfrow=c(1,1))



