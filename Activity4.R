#Activity 4
#Isabel Drexler Booth
#dplyr::filter(stuff)
#stats::filter() <-- avoids conflict errors

#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(tidyverse)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables
#subsets iris data to only look at flowers in versicolor species
irisV <- subset(iris, Species == "versicolor")

regression_variables <- c(irisV$Sepal.Length~irisV$Sepal.Width, irisV$Petal.Length~irisV$Petal.Width, irisV$Sepal.Length~irisV$Petal.Length)
reg_tables <- list()

#loops through list to make a regression table for every comparison
for (i in 1:length(regression_variables)){
  model <- lm(regression_variables[[i]])
  reg_tables[[i]] <- model
}

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#this dplyr function adds info from the second data frame to the left side of the first data frame (first and second of the arguments)
irisH <- dplyr::left_join(iris, height)


#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)
par(mfrow=c(1,2))

#3a. now make the same plot in ggplot
ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point(shape = 1, size = 2)

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point(shape = 1, size = 2) + theme_classic()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species, size = Petal.Length)) + geom_point() + theme_classic() +
    labs(x = "Sepal Length", y = "Sepal Width", title = "Scatterplot of Iris Sepal Length by Iris Sepal Width")

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################	
#both have different syntax. With plot, all arguments and ways of formatting the graph need to be included in the plot arguments, 
#     but with ggplot you build the plot with individual, specialized functions. 
#     Additionally, the argument variable names are a lot more intuitive and built in than those with plot.
#     ggplot is easier to code with because it's easier to build on an original plot without actively changing the code for the original plot.