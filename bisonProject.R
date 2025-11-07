#Isabel Drexler Booth
library(tidyverse)


#READ IN DATA ON PC
#read in oak sapling growth data
#datGrowth <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\knb-lter-cdr.703.1\\e321_Oak sapling growth.txt",
#                      header = TRUE)
#read in carbon and nitrogen percent
#datPercent <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\knb-lter-cdr.703.1\\e321_Soil Carbon and Nitrogen.txt",
#                        header = TRUE)

#READ IN DATA ON MAC
#read in oak sapling growth data
datGrowth <- read.delim("/Volumes/CLASSspace/GEOG331_F25/idrexlerbooth/data/project_data/e321_Oak sapling growth.txt",
                      header = TRUE)
#read in carbon and nitrogen percent
datPercent <- read.delim("/Volumes/CLASSspace/GEOG331_F25/idrexlerbooth/data/project_data/e321_Soil Carbon and Nitrogen.txt",
                         header = TRUE)
#read in land cover
datCover <- read.delim("/Volumes/CLASSspace/GEOG331_F25/idrexlerbooth/data/project_data/e321_Species Percent Cover.txt",
                         header = TRUE)

#make a boxplot
datGrowth$bison_fence <- as.factor(datGrowth$bison_fence)
ggplot(datGrowth, aes(x = bison_fence, y = height, fill = bison_fence)) + geom_boxplot() +
  labs(title = "Oak Sapling Height of Plots Inside and Outside of the Bison Enclosures",
       x = "Within the Bison Plot",
       y = "Oak Sapling Height") +
  #this notation to label individual boxplots from Stack Overflow (https://stackoverflow.com/questions/67203268/x-axis-labels-ggplot2-in-r)
  scale_x_discrete(
    name="Location around Bison Fence",
    labels=c("no" = "Outside", 'yes'= 'Inside')) +
  #I didn't like the legend, so google generated answer says that this will get rid of it
    guides(fill = "none") +
    theme_classic()

#testing out other boxplots, as this does not appear significant
datGrowth$grazed <- as.factor(datGrowth$grazed)
ggplot(datGrowth, aes(x = grazed, y = height)) + geom_boxplot()

ggplot(datGrowth, aes(x = bison_fence, y = diameter)) + geom_boxplot()
ggplot(datGrowth, aes(x = grazed, y = diameter)) + geom_boxplot() 

datPercent$bison_fence <- as.factor(datPercent$bison_fence)
ggplot(datPercent, aes(x = bison_fence, y = TC_perC)) + geom_boxplot()

datPercent$grazed <- as.factor(datPercent$grazed)
ggplot(datPercent, aes(x = grazed, y = TC_perC)) + geom_boxplot()
