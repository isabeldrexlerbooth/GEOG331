#Isabel Drexler Booth
library(tidyverse)


#READ IN DATA ON PC
#read in oak sapling growth data
datGrowth <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Oak sapling growth.txt", header = TRUE)
#read in carbon and nitrogen percent
datPercent <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Soil Carbon and Nitrogen.txt", header = TRUE)
#read in land cover
datCover <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Species Percent Cover.txt", header = TRUE)
#read in consumption
datConsumption <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Consumption.txt", header = TRUE)
#read in aboveground biomass
datBiomass <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Aboveground Biomass.txt", header = TRUE)


#READ IN DATA ON MAC
#read in oak sapling growth data
#datGrowth <- read.delim("/Volumes/CLASSspace/GEOG331_F25/idrexlerbooth/data/project_data/e321_Oak sapling growth.txt", header = TRUE)
#read in carbon and nitrogen percent
#datPercent <- read.delim("/Volumes/CLASSspace/GEOG331_F25/idrexlerbooth/data/project_data/e321_Soil Carbon and Nitrogen.txt", header = TRUE)
#read in land cover
#datCover <- read.delim("/Volumes/CLASSspace/GEOG331_F25/idrexlerbooth/data/project_data/e321_Species Percent Cover.txt", header = TRUE)

#FACTORING THE GRAZED/UNGRAZED WITH INSIDE/OUTSIDE FENCE
#not doing cover because it's looking useless
#add a new column with yes/yes, yes/no, no/yes, and no/no to each dataset, then factorize it for that dataset
#HOW IT'S ORGANIZED ------- fence/grazed
#first up, oak sapling growth
datGrowth$crossover <- paste(datGrowth$bison_fence, datGrowth$grazed, sep = "/")
growthCrossover <- as.factor(datGrowth$crossover)
growthCrossover2 <- factor(growthCrossover, ordered = FALSE)

#make the scatterplot -- NEED TO PUT THIS IN ITS PLACE
ggplot(datGrowth, aes(diameter, height, color = growthCrossover2)) + geom_point() + theme_classic() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Oak Sapling Diameter", y = "Oak Sapling Height", title = "Oak Sapling Diameter and Height") +
  scale_color_manual(labels = c("outside of the bison fence and not grazed", "inside the bison fence and not grazed", "inside the bison fence and grazed") , values = c("olivedrab", "olivedrab3", "orange2")) +
  theme_bw() +
  guides(color=guide_legend("Location by Bison Fence and Grazing Status"))
#still need to change key labels!!!

#12/4
regNN <- lm(height ~ diameter + growthCrossover2, data = datGrowth)
summary(regNN)

print(is.ordered(growthCrossover2))
class(growthCrossover)
datGrowth$YN <- relevel(datGrowth$growthCrossover2, ref = "yes/no")
regYN 

regYY


#lets try this again
regressions <- datGrowth %>%
  group_by(crossover) %>%
  do(model = lm(height~diameter, data = .))
coefficient <- regressions %>%
  rowwise() %>%
  summarize(
    factor_level = crossover,
    intercept = coef(model)[1],
    slope = coef(model)[2]
  )
print(coefficients)
nn <- regressions$model[[which(regressions$crossover == "no/no")]]
summary(nn)


#11/20
#MAKING A LINE GRAPH TO TRACK GREEN MATTER CONSUMPTION OVER 4 SAMPLING PERIODS (Bar chart bc time is discrete?)
#factoring time period
timePer <- as.factor(datConsumption$sample_period)
#factoring fenced status
fenceStatus <- as.factor(datConsumption$subplot)
#factoring species type
speciesCons <- as.factor(datConsumption$species)

#get mean mass consumed for every species-period-bison combo
aveMass <- aggregate(mass ~ timePer + fenceStatus + species, data = datConsumption, FUN = mean)

#THIS GOT CALLED OFF BECAUSE OF NOT GREAT DATA


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

