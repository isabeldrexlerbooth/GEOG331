######################################
# Isabel Drexler Booth
# examples of linear regression using built in IRIS data
# 10/7/2025
#######################################################

rm(list =  ls())

#subset the Virginica species
flower <- iris[iris$Species == 'virginica',]

#make sepal length vs petal length scatter plot
plot(flower$Sepal.Length, flower$Petal.Length, pch = 19,
     xlab = "Sepal Length", ylab = "Petal Length",
     main = "Iris virginica")

#fit a regression model
fit<- lm(flower$Petal.Length ~ flower$Sepal.Length)

#plot the residuals
plot(flower$Sepal.Length, summary(fit)$residuals, pch = 19,
     xlab = "Sepal Length", ylab = "Residuals")
     abline(h = 0)

#check normality of residuals
hist(summary(fit)$residuals, col = "red",
     main = "Residual Distribution", xlab = "Residuals")

#qqnorm or qq line can provide another visual check
qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals, pch = 19)

#user Shapiro wilks tet to check normality
shapiro.test(summary(fit)$residuals)
