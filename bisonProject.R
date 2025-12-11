#Isabel Drexler Booth
library(tidyverse)
library(FSA)
library(nparcomp)

#READ IN DATA ON PC
#read in oak sapling growth data
#datGrowth <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Oak sapling growth.txt", header = TRUE)
#read in carbon and nitrogen percent
#datPercent <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Soil Carbon and Nitrogen.txt", header = TRUE)
#read in land cover
#datCover <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Species Percent Cover.txt", header = TRUE)
#read in consumption
#datConsumption <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Consumption.txt", header = TRUE)
#read in aboveground biomass
#datBiomass <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Aboveground Biomass.txt", header = TRUE)


#READ IN DATA ON MAC
#read in oak sapling growth data
datGrowth <- read.delim("/Volumes/GEOG331_F25/idrexlerbooth/data/project_data/e321_Oak sapling growth.txt", header = TRUE)
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
  labs(x = "Oak Sapling Diameter", y = "Oak Sapling Height", title = "Comparison of Oak Sapling Diameter and Height") +
  scale_color_manual(labels = c("excluded and not grazed", "included and not grazed", "included and grazed") , values = c("olivedrab", "olivedrab3", "orange2")) +
  theme_bw() + theme(legend.position = "bottom") +
  guides(color=guide_legend("Bison Enclosure and Grazing Status"))
#still need to change key labels!!!


#12/4
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
confint(nn)
yn <- regressions$model[[which(regressions$crossover == "yes/no")]]
summary(yn)
confint(yn)
yy <- regressions$model[[which(regressions$crossover == "yes/yes")]]
summary(yy)
confint(yy)
modelOverall <- lm(height ~ diameter, data = datGrowth)
summary(modelOverall)

model1 <- lm(height ~ diameter + crossover, data = datGrowth)
summary(model1)
datGrowth$crossover <- relevel(datGrowth$crossover, ref = "yes/yes")
model1 <- lm(height ~ diameter + crossover, data = datGrowth)
summary(model1)
###use forcats for this!!!


#shapiro wilk for normaility
#height is very not normal
# Perform Shapiro-Wilk test for 'Value' grouped by 'Factor'
shapiro_results_dplyr <- datGrowth %>%
  group_by(crossover) %>%
  summarize(shapiro_test = list(shapiro.test(height)))
# Extract and print p-values
shapiro_results_dplyr %>%
  mutate(p_value = sapply(shapiro_test, function(x) x$p.value)) %>%
  select(crossover, p_value)
####so is diameter
shapiro_results_dplyr <- datGrowth %>%
  group_by(crossover) %>%
  summarize(shapiro_test = list(shapiro.test(diameter)))
# Extract and print p-values
shapiro_results_dplyr %>%
  mutate(p_value = sapply(shapiro_test, function(x) x$p.value)) %>%
  select(crossover, p_value)

#now for the ANOVAs -- NOPE
#anova_diam <- aov(diameter ~ crossover, data = datGrowth)
#summary(anova_diam)
#anova_height <- aov(height ~ crossover, data = datGrowth)
#summary(anova_height)

#Using Kruskal Wallis instead
resultDiam <- kruskal.test(diameter ~ crossover, data = datGrowth)
print(resultDiam)
resultHeight <- kruskal.test(height ~ crossover, data = datGrowth)
print(resultHeight)

#quartiles for table
#quartDiam <- quantile(datGrowth, probs = c(0.25, 0.50, 0.75))
#print(quartiles)
datGrowth %>%
  group_by(crossover) %>%
  summarise(
    n = n(),
    Q1 = quantile(diameter, 0.25, na.rm = TRUE),
    Median = quantile(diameter, 0.50, na.rm = TRUE),
    Q3 = quantile(diameter, 0.75, na.rm = TRUE)
  )
datGrowth %>%
  group_by(crossover) %>%
  summarise(
    n = n(),
    Q1 = quantile(height, 0.25, na.rm = TRUE),
    Median = quantile(height, 0.50, na.rm = TRUE),
    Q3 = quantile(height, 0.75, na.rm = TRUE)
  )




#height is almost not really significant, so we are performing a post-hoc dunns test
dunnTest(height ~ crossover,
         data=datGrowth,
         method="bonferroni")

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


#This are the two I'm going to want to do (BOXPLOTS YAYAYAYAY!!!)
ggplot(data = datGrowth, aes(x = crossover, y = diameter)) +
  geom_boxplot()
ggplot(data = datGrowth, aes(x = crossover, y = height)) +
  geom_boxplot()
#height fancy
ggplot(datGrowth, aes(x = crossover, y = height, fill = crossover)) + geom_boxplot() +
  labs(title = "Oak Sapling Height at Plot Enclosure and Grazing Status",
       x = "Bison Enclosure and Grazing Status",
       y = "Oak Sapling Height") + 
  #this notation to label individual boxplots from Stack Overflow (https://stackoverflow.com/questions/67203268/x-axis-labels-ggplot2-in-r)
  scale_x_discrete(
    name="Bison Enclosure and Grazing Status",
    labels=c("no/no" = 'Excluded and not grazed', 'yes/no'= 'Included and not grazed', 'yes/yes' = 'Included and grazed')) +
  #I didn't like the legend, so google generated answer says that this will get rid of it
  scale_fill_manual(values = c("no/no" = "olivedrab", "yes/no" = "olivedrab3", "yes/yes" = "orange2")) +
  guides(fill = "none") +
  theme_classic()
#diameter fancy
ggplot(datGrowth, aes(x = crossover, y = diameter, fill = crossover)) + geom_boxplot() +
  labs(title = "Oak Sapling Diameter at Plot Enclosure and Grazing Status",
       x = "Bison Enclosure and Grazing Status",
       y = "Oak Sapling Diameter") + 
  #this notation to label individual boxplots from Stack Overflow (https://stackoverflow.com/questions/67203268/x-axis-labels-ggplot2-in-r)
  scale_x_discrete(
    name="Bison Enclosure and Grazing Status",
    labels=c("no/no" = 'Excluded and not grazed', 'yes/no'= 'Included and not grazed', 'yes/yes' = 'Included and grazed')) +
  #I didn't like the legend, so google generated answer says that this will get rid of it
  scale_fill_manual(values = c("no/no" = "olivedrab", "yes/no" = "olivedrab3", "yes/yes" = "orange2")) +
  guides(fill = "none") +
  theme_classic()
