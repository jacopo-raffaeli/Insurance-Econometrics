rm(list = ls()) # for cleaning

### Load packages
library(foreign)
library(xts)
library(ggplot2)
#library(stargazer)

library(BatchGetSymbols)

# Otherwise you can use
# if(!require(BatchGetSymbols)) install.packages('BatchGetSymbols')

#This function shows the current working directory of your file.
getwd



##################################
##### Answer to question 1 #######
##################################


#This function opens the data set which is a dta file (Stata)
mydata <- read.dta("CAPM2019.dta")
mydata <- read.dta(file.choose())

## or
# data <- file.choose()
# mydata <- read.dta(data)

head(mydata, 3)

boxplot(mydata[-1,],main=" ", xlab= "Securities", ylab="Return")

ret_race <- mydata[,"rrace"]
head(ret_race)

ret_nsdq <- mydata[,"rndx"]
head(ret_nsdq)

ret_tbill <- mydata[,"rtbill"]
head(ret_tbill)
x11()
plot(ret_race, type = "l", col = "red", lwd = 2)
plot(ret_nsdq, type = "l", col = "blue", lwd = 2)
plot(ret_tbill, type = "l", col = "orange", lwd = 2)


##################################
##### Answer to question 2 #######
##################################


er_ferr <- ret_race - ret_tbill
head(er_ferr)
summary(er_ferr)

er_nsdq <- ret_nsdq - ret_tbill
head(er_nsdq)
summary(er_nsdq)


##################################
##### Answer to question 3 #######
##################################

?lm
fit <- lm(er_ferr ~ er_nsdq)
summary(fit)
res=fit$residuals
#Regression Hypothesis
#1)
mean(res)
#2) Homoschedastic?
x11()
plot(res)
library(whitestrap)
white_test(fit)
#3) Independence
plot(res[1:length(res)-1],res[2:length(res)])
library(lmtest)

dwtest(fit) #Durbin Watson

#4) OK
#5) Gaussianity
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 
# Using the following function we get Latex output ready
library(tseries)
jarque.bera.test(res)

plot(er_nsdq,er_ferr, xlim=c(-0.05,0.05),ylim=c(-0.05,0.05), main="",
     col = "orange")

beta <- round(summary(fit)$coefficients[2, 1],4)
abline(fit, col="blue")
text(-0.05, 0.04, paste("Beta =", beta), pos = 4)

# Comments:
#
# Alpha
#
# Notice that alpha's p-value is 6% -> we cannot reject H0 at 5% significance level
# Therefore, we accept H0, i.e. we accept alpha=0
#
# Beta
#
# We consider a significance level of 5%. The p-value of the estimated coefficient 
# relative to er_nsdq is 8.97e-14 -> we reject H0: ??=0 in favor of H1: ?? !=0
# Therefore, ?? is different from zero.
# This means that the indipendent explanatory variable is able to influence the 
# dependent variable. 
#
# Moreover, ??=0.897. This means that for a change of 0.1 in er_nsdq we can observe 
# a change of 0.0897 in er_ferr.
# Finally, since ??<1, the expected net return for Ferrari is lower than the return
# of the market and correspondingly the security is less risky.


# We can compare fitted values
?lines
plot(er_ferr[2:41],type="l",col="green", ylab = "Excess Return",
     main = "Actual vs Fitted")
lines(fitted(fit),col="blue")


# We can compute the fitted values also "manually"

# beta
er_nsdq.na <- na.omit(er_nsdq)
er_ferr.na <- na.omit(er_ferr)
beta <- sum((er_nsdq.na - mean(er_nsdq.na)) * (er_ferr.na - mean(er_ferr.na))) /
  sum((er_nsdq.na - mean(er_nsdq.na))^2)
beta

# alpha
alpha <- mean(er_ferr.na) - beta * mean(er_nsdq.na)
alpha

# Compute SSR (Sum of Squared Residuals)
# extract fitted values
predicted <- fitted(fit)

# compute the squared deviation between actual values and fitted values
dev <- (er_ferr.na - predicted)^2

# sum squared deviations
RSS <- sum(dev)
RSS

# or
rss <- sum(fit$residuals^2)
rss

# Compute Standard error of the regression

SER <- sqrt(RSS/(length(er_ferr.na)-2))
SER

length(er_ferr.na)-2

# Standard error for alpha and beta

se_beta <- SER/sqrt(sum((er_nsdq.na - mean(er_nsdq.na))^2))
se_beta

se_alpha <- SER*(sqrt((1/length(er_nsdq.na)) + (mean(er_nsdq.na)^2)/
                        sum((er_nsdq.na - mean(er_nsdq.na))^2)))
se_alpha

coef(summary(fit))[2, "Std. Error"]

coef(summary(fit))[1, "Std. Error"]


##################################
##### Answer to question 4 #######
##################################

# a) first method
?confint
confint(fit, level = 0.95)

# b) second method
df = fit$df.residual

### Intercept
# Upper Bound
fit$coefficients[1]+qt(0.975, df=df)*coef(summary(fit))[1, "Std. Error"]

# Lower Bound
fit$coefficients[1]-qt(0.975, df=df)*coef(summary(fit))[1, "Std. Error"]

### er_nsdq
# Upper Bound
fit$coefficients[2]+qt(0.975, df=df)*coef(summary(fit))[2, "Std. Error"]

# Lower Bound
fit$coefficients[2]-qt(0.975, df=df)*coef(summary(fit))[2, "Std. Error"]


##################################
##### Answer to question 5 #######
##################################

# Two tailed test
# H0: alpha = 0

alpha_test <- abs((fit$coefficients[1]-0)/coef(summary(fit))[1, "Std. Error"])
alpha_test

# We reject H0 if alpha_test > qt(0.975)

df

qt(0.975, df= df)

alpha_test > qt(0.975, df= df)


# One tailed test
# H0: beta = 1
# H1: beta > 1

beta_test <- (fit$coefficients[2]-1)/coef(summary(fit))[2, "Std. Error"]
beta_test

p_value <- 1 - pt(beta_test, df)

df # degrees of freedom

qt(0.95, df= df)

beta_test > qt(0.95, df= df)

# H1: beta < 1

beta_test < qt(0.05, df= df)

p_value <- pt(beta_test, df)

##################################
##### Answer to question 6 #######
##################################


predicted <- fitted(fit)
tss <- sum((er_ferr.na - mean(er_ferr.na))^2)
ess <- sum((predicted - mean(er_ferr.na))^2)
r2 <- ess/tss
r2




# R^2 measures how well the OLS regression line fits data and it is the most common 
# measure of fit. Intuitively it is the fraction of variance of Yi that is explained by Xi.
# The range of R^2 is between 0 and 1. If R^2 is equal to 1, this means that the model
# is able to explain data in a perfect way.
# In our case, the model is able to explain the 77.2% of total variability.



# How to download data?
# BatchGetSymbols

library(BatchGetSymbols)

### set dates
# first.date <- Sys.Date() - 60
# last.date <- Sys.Date()
# OR
first.date <- "2022-01-01"
last.date <- "2022-03-23"

# a) Ferrari ("RACE")
tickers <- c('AMZN')
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)
str(l.out)
x11()
plot(x= l.out$df.tickers$ref.date, y = l.out$df.tickers$price.close, type = "l",
     main = "Race", xlab = "ref.date", ylab="price.close", col = "red",
     col.axis = "red", lwd = 2)


