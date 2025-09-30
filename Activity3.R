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
#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently (is not read as a character)
datW <- read.csv("Z:\\idrexlerbooth\\data\\bewkes\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("Z:\\idrexlerbooth\\data\\bewkes\\bewkes_weather.csv",
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

