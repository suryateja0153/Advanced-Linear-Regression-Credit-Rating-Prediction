#Author: Suryateja Chalapati

#Importing libraries
library(readxl)
library(corrplot)
library(stargazer)

setwd('C:/Users/surya/Downloads')
cr = read_excel("CreditRating.xlsx", sheet = 'Data')

#NA values column wise
sapply(cr, function(x) sum(is.na(x)))
str(cr)

#Basic statistics
summary(cr)

#Credit rating can be between 300 and 850
cr_fil = cr[!(cr$Rating < 300 | cr$Rating > 850),]

hist(cr_fil$Rating, prob=T, main = "Histogram of Credit Rating", col = 'green')
density_1 = density(cr_fil$Rating)                    
lines(density_1, col="red")

plot(cr_fil$Income, cr_fil$Rating, main='Scatterplot of Income and Credit Rating',
     col = 'blue', xlab = "Income", ylab='Rating')
abline(lm(Rating ~ Income, data = cr_fil), col = "red")

plot(cr_fil$Limit, cr_fil$Rating, main='Scatterplot of Credit Limit and Credit Rating',
     col = 'purple', xlab = "Limit", ylab='Rating')
abline(lm(Rating ~ Limit, data = cr_fil), col = "black")

#Checking correlation
cr1 = subset(cr_fil, select = c(Income, Limit, Rating, Cards, Balance))
cr1_corr = cor(cr1)
corrplot(cr1_corr, method = "number")
#Credit rating and limit are directly proportional

#1st linear model
m1 <- lm(Rating ~ Gender + Education + Married + Ethnicity, data=cr)
summary(m1)

par(mfrow = c(2, 2))
plot(m1)
par(mfrow=c(1,1))

#Residuals seems to follow an inverted U pattern which implies non-linearity
#2nd linear model
m2 <- lm(Rating ~ Income + Cards + Age + Student + Balance, data = cr)
summary(m2)

par(mfrow = c(2, 2))
plot(m2)
par(mfrow=c(1,1))

#3rd linear model
m3 <- lm(Rating ~ Income + Cards + Age + Student + Balance + Gender + Education + Married + Ethnicity, data = cr)
summary(m3)

par(mfrow = c(2, 2))
plot(m3)
par(mfrow=c(1,1))

#Stargazer
stargazer(m1, m2, type='text', single.row = TRUE)

stargazer(m1, m2, type='text', ci=TRUE, ci.level=0.95, single.row = TRUE)

m = mean(cr$Cards)
m
sd(cr$Cards)
err = 0.918

lb = err - m
lb

ub = err + m
ub

NROW(cr$Cards)

