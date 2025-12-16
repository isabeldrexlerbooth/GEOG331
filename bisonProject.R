#Isabel Drexler Booth
#Environmental Data Science Investigation of bison rewilding on oak sapling growth in oak savannas
library(tidyverse)
library(FSA)

#READ IN DATA ON PC
#read in oak sapling growth data
#datGrowth <- read.delim("Z:\\idrexlerbooth\\data\\project_data\\e321_Oak sapling growth.txt", header = TRUE)

#READ IN DATA ON MAC
#read in oak sapling growth data
datGrowth <- read.delim("/Volumes/GEOG331_F25/idrexlerbooth/data/project_data/e321_Oak sapling growth.txt", header = TRUE)

#FACTORING THE GRAZED/UNGRAZED WITH INSIDE/OUTSIDE FENCE
#not doing cover because it's looking useless
#add a new column with yes/yes, yes/no and no/no to each dataset, then factorize it for that dataset. There is no , no/yes factor, as it is impossible for a plot to be outside of the bison enclosure, but still experience bison grazing
#HOW IT'S ORGANIZED ------- fence/grazed
datGrowth$crossover <- paste(datGrowth$bison_fence, datGrowth$grazed, sep = "/")
growthCrossover <- as.factor(datGrowth$crossover)
growthCrossover2 <- factor(growthCrossover, ordered = FALSE)

#make a scatterplot comparing relationship between diameter and height with regression lines
ggplot(datGrowth, aes(diameter, height, color = growthCrossover2)) + geom_point() + theme_classic() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Oak Sapling Diameter", y = "Oak Sapling Height", title = "Comparison of Oak Sapling Diameter and Height") +
  scale_color_manual(labels = c("excluded and not grazed", "included and not grazed", "included and grazed") , values = c("olivedrab", "olivedrab3", "orange2")) +
  theme_bw() + theme(legend.position = "bottom") +
  guides(color=guide_legend("Bison Enclosure and Grazing Status"))

#12/4
#generate regression models and equations for each of the treatments as imaged in the scatterplot
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

#getting and printing regression equations and 95% confidence intervals for slope and intercept
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


#make boxplots to show variation in measurements at each treatment level
#This are the two I'm going to want to do (BOXPLOTS YAYAYAYAY!!!)
#height fancy
ggplot(datGrowth, aes(x = crossover, y = height, fill = crossover)) + geom_boxplot() +
  labs(title = "Oak Sapling Height at Plot Enclosure and Grazing Status",
       x = "Bison Enclosure and Grazing Status",
       y = "Oak Sapling Height") + 
  #this notation to label individual boxplots from Stack Overflow (https://stackoverflow.com/questions/67203268/x-axis-labels-ggplot2-in-r)
  scale_x_discrete(
    name="Bison Enclosure and Grazing Status",
    labels=c("no/no" = 'Excluded and not grazed', 'yes/no'= 'Included and not grazed', 'yes/yes' = 'Included and grazed')) +
  scale_fill_manual(values = c("no/no" = "olivedrab", "yes/no" = "olivedrab3", "yes/yes" = "orange2")) +
  guides(fill = "none") +
  theme_classic()
#diameter fancy
ggplot(datGrowth, aes(x = crossover, y = diameter, fill = crossover)) + geom_boxplot() +
  labs(title = "Oak Sapling Diameter at Plot Enclosure and Grazing Status",
       x = "Bison Enclosure and Grazing Status",
       y = "Oak Sapling Diameter") + 
  scale_x_discrete(
    name="Bison Enclosure and Grazing Status",
    labels=c("no/no" = 'Excluded and not grazed', 'yes/no'= 'Included and not grazed', 'yes/yes' = 'Included and grazed')) +
  scale_fill_manual(values = c("no/no" = "olivedrab", "yes/no" = "olivedrab3", "yes/yes" = "orange2")) +
  guides(fill = "none") +
  theme_classic()

#quartiles for boxplots, this is used in the Kruskal-Wallis table
#diameter IQR
datGrowth %>%
  group_by(crossover) %>%
  summarise(
    n = n(),
    Q1 = quantile(diameter, 0.25, na.rm = TRUE),
    Median = quantile(diameter, 0.50, na.rm = TRUE),
    Q3 = quantile(diameter, 0.75, na.rm = TRUE)
  )
#height IQR
datGrowth %>%
  group_by(crossover) %>%
  summarise(
    n = n(),
    Q1 = quantile(height, 0.25, na.rm = TRUE),
    Median = quantile(height, 0.50, na.rm = TRUE),
    Q3 = quantile(height, 0.75, na.rm = TRUE)
  )

#shapiro-wilk for normaility. A p value > 0.05 indicated a normal distribution
#normality of height measurements by treatment
shapiro_results_dplyr <- datGrowth %>%
  group_by(crossover) %>%
  summarize(shapiro_test = list(shapiro.test(height)))
#printing p values
shapiro_results_dplyr %>%
  mutate(p_value = sapply(shapiro_test, function(x) x$p.value))
#normality of diameter measurements by treatment
shapiro_results_dplyr <- datGrowth %>%
  group_by(crossover) %>%
  summarize(shapiro_test = list(shapiro.test(diameter)))
#printing p values
shapiro_results_dplyr %>%
  mutate(p_value = sapply(shapiro_test, function(x) x$p.value))

#Because data is not normally distributed, we cannot use an anova test, and therefore select a Kruskal-Wallis

#Using Kruskal-Wallis instead
resultDiam <- kruskal.test(diameter ~ crossover, data = datGrowth)
print(resultDiam)
resultHeight <- kruskal.test(height ~ crossover, data = datGrowth)
print(resultHeight)

#height is almost not significant, so we are performing a post-hoc dunns test to see if a treatment is actually significantly different for height
#the bonferroni correction decreases Type 1 error
dunnTest(height ~ crossover,
         data=datGrowth,
         method="bonferroni")


# THE BELOW CODE IN UNNECCESSARY FOR THE ACTUAL RESULTS PRESENTED IN THE PAPER, BUT SHOWS A LITTLE BIT OF THE PROCESS, SO I THOUGHT I WOULD COMMENT IT OUT AND LEAVE IT HERE
# 
# initial boxplots from the first figure generated
# datGrowth$bison_fence <- as.factor(datGrowth$bison_fence)
# ggplot(datGrowth, aes(x = bison_fence, y = height, fill = bison_fence)) + geom_boxplot() +
#   labs(title = "Oak Sapling Height of Plots Inside and Outside of the Bison Enclosures",
#        x = "Within the Bison Plot",
#        y = "Oak Sapling Height") +
#   #this notation to label individual boxplots from Stack Overflow (https://stackoverflow.com/questions/67203268/x-axis-labels-ggplot2-in-r)
#   scale_x_discrete(
#     name="Location around Bison Fence",
#     labels=c("no" = "Outside", 'yes'= 'Inside')) +
#   #I didn't like the legend, so google generated answer says that this will get rid of it
#   guides(fill = "none") +
#   theme_classic()
# this boxplot did not seem to show much of a difference between plot treatments, 
#       so I tried generating other boxplots with other datasets from the experiment that have sense been ruled out, as they were not fit for analysis/I ended up not using them
#testing out other boxplots, as this does not appear significant -- MORE FORMALIZED BOXPLOTS DEVELOPED LATER
# datGrowth$grazed <- as.factor(datGrowth$grazed)
# ggplot(datGrowth, aes(x = grazed, y = height)) + geom_boxplot()
# ggplot(datGrowth, aes(x = bison_fence, y = diameter)) + geom_boxplot()
# ggplot(datGrowth, aes(x = grazed, y = diameter)) + geom_boxplot() 
# datPercent$bison_fence <- as.factor(datPercent$bison_fence)
# ggplot(datPercent, aes(x = bison_fence, y = TC_perC)) + geom_boxplot()
# datPercent$grazed <- as.factor(datPercent$grazed)
# ggplot(datPercent, aes(x = grazed, y = TC_perC)) + geom_boxplot()

#11/20 -- This day in class was mostly spent trying to generate the code in class, it however was never used because we (Professor Loranty and I) were unsure how consumption was being measured, making it difficult to interpret
#MAKING A LINE GRAPH TO TRACK GREEN MATTER CONSUMPTION OVER 4 SAMPLING PERIODS (Bar chart bc time is discrete?)
#this testing was abandoned, as the consumption data was deemed unworkable due to lacking metadata.
#factoring time period
#timePer <- as.factor(datConsumption$sample_period)
#factoring fenced status
#fenceStatus <- as.factor(datConsumption$subplot)
#factoring species type
#speciesCons <- as.factor(datConsumption$species)

#get mean mass consumed for every species-period-bison combo
#aveMass <- aggregate(mass ~ timePer + fenceStatus + species, data = datConsumption, FUN = mean)

#THIS GOT CALLED OFF BECAUSE OF NOT GREAT DATA