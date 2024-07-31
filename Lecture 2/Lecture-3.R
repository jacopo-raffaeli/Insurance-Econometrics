# install.packages("car")
library(car)
### Line of code which checks if the package is already installed
if(!require(car)) install.packages("car")

#?file.choose #choose a file interactively.
hprice <- read.csv('hprice.data.txt',header=TRUE,sep=" ")

?dim
dim(hprice)
class(hprice)

?objects
objects(hprice)

View(hprice)

?str
str(hprice)

head(hprice$saleprice)
head(hprice)

summary(hprice)


##################################
##### Answer to question 1 #######
##################################

?par
x11()
par(mfrow=c(2,2))
plot(hprice$saleprice, hprice$lotsize , main="sale price and lot size",
     col.main="red", col = "red")

plot(hprice$saleprice, hprice$bedroom, main="sale price and bedroom",
     col.main="green", col= "green")

plot(hprice$saleprice, hprice$bath, main="sale price and bath",
     col.main="blue", col= "blue")

plot(hprice$saleprice, hprice$stories, main="sale price and stories",
     col.main="orange", col= "orange")

plot(hprice$saleprice, hprice$driveway, main="sale price and driveway",
     col.main="brown", col = "brown")

plot(hprice$saleprice, hprice$recroom, main="sale price and recroom",
     col.main="black", col = "black")

plot(hprice$saleprice, hprice$basement, main="sale price and basement",
     col.main="red", col = "red")

plot(hprice$saleprice, hprice$garage, main="sale price and garage",
     col.main="orange", col = "orange")

plot(hprice$saleprice, hprice$desireloc, main="sale price and desireloc",
     col.main="purple", col = "purple")

plot(hprice$saleprice, hprice$gas, main="sale price and gas",
     col.main="blue", col ="blue")

plot(hprice$saleprice, hprice$aircond, main="sale price and aircond",
    col.main="green", col= "green")



dev.off()


##################################
##### Answer to question 2 #######
##################################

fit <- lm(hprice$saleprice ~ hprice$lotsize + hprice$bedroom + hprice$bath
          + hprice$stories)
summary(fit)
res=fit$residuals
#Regression Hypothesis
#1)
mean(res)
#2) Homoschedastic?
x11()
x11()
par(mfrow=c(2,2))

plot( hprice$lotsize,res)
plot( hprice$bedroom,res)
plot( hprice$bath,res)
plot( hprice$stories,res)
dev.off()

library(whitestrap)
white_test(fit)
#3) Independence/ We can skip autocorrelation it is not a time-series


#4) OK
#5) Gaussianity
x11()
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 
# Using the following function we get Latex output ready
library(tseries)
jarque.bera.test(res)


######## Try winsorize at 5%
library(DescTools)

winsorized=lapply(hprice, Winsorize)
fit <- lm(saleprice ~ lotsize + bedroom + bath
          + stories,winsorized)
summary(fit)
res=fit$residuals
#Regression Hypothesis
#1)
mean(res)
#2) Homoschedastic?
x11()
par(mfrow=c(2,2))

plot( hprice$lotsize,res)
plot( hprice$bedroom,res)
plot( hprice$bath,res)
plot( hprice$stories,res)
dev.off()
library(whitestrap)
white_test(fit)
#3) Independence

#4) OK
#5) Gaussianity
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 
# Using the following function we get Latex output ready
library(tseries)
jarque.bera.test(res)

######################### Cook's Distance
library(MASS)

library(car)

fit_old <- lm(saleprice ~ lotsize + bedroom + bath
          + stories,winsorized)
ix=cooks.distance(fit_old)<4/(length(cooks.distance(fit_old)))
hprice_free=data.frame(winsorized)[ix,]
fit <- lm(saleprice ~ lotsize + bedroom + bath
          + stories,hprice_free)
summary(fit)


res=fit$residuals
#Regression Hypothesis
#1)
mean(res)
#2) Homoschedastic?
x11()
par(mfrow=c(2,2))

plot( hprice_free$lotsize,res)
plot( hprice_free$bedroom,res)
plot( hprice_free$bath,res)
plot( hprice_free$stories,res)
dev.off()
white_test(fit)
#3) Independence

#4) OK
#5) Gaussianity
x11()
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

jarque.bera.test(res)

  ##################################
##### Answer to question 3 #######
##################################

#
# Intercept: Considering a significance level of 5% and the p-value (0.2663), the
# evidence to reject the null hypothesis is weak and consequently we accept the null
# hypothesis (H0 : ??0 = 0). Therefore, we may say ??0 = 0.

# Regressors: Considering a significance level of 5% and the p-value of the coefficients 
# associated with all the explanatory variables (x1i, x2i, x3i, x4i), we can
# reject the null hypothesis (H0 : ??j = 0, j = 1, 2, 3, 4) in favor of the alternative
# hypothesis (H1 : ??j != 0). Hence, ??1, ??2, ??3 and ??4 are statistically significant.


##################################
##### Answer to question 4 #######
##################################

# ! In multiple regression it is important the concept of ceteris paribus

# Lot size of the property
#
# An extra square foot to the size of the house will tend to add $6.19 to
# the house price, ceteris paribus.
# If we consider houses with the same number of bedrooms, bathrooms
# and storeys, an extra square foot to the size of the house will tend to
# add $6.19 to the house price.
# If we compare houses with the same number of bedrooms, bathrooms
# and storeys, those with larger size will tend to be worth more. 

# Number of bedrooms
#
# Houses with an extra bedroom will tend to be worth $2298 more
# than houses without the extra bedroom, ceteris paribus.

# Number of bathrooms
#
# Houses with an extra bathroom will tend to be worth $16305 more
# than houses without the extra bathroom, ceteris paribus.

# Number of storeys (excluding the basement)
#
# If we compare houses with the same characteristics (similar in all other
# aspects), those with an extra storey will tend to be worth $6853.90
# more.


##################################
##### Answer to question 5 #######
##################################

# 3 methods:

# 1
fit <- lm(saleprice ~ lotsize + bedroom + bath
          + stories,hprice_free)
summary(fit)

# 2
library(car)
?linearHypothesis
linearHypothesis(fit, c("lotsize=0", "bath=0",
                        "stories=0", "bedroom=0"))

# 3 (manually)

rss_unrestricted <- round(sum(fit$residuals^2)/(10^11),4)
rss_unrestricted

fit3 <- lm(hprice_free$saleprice ~ 1)
rss_restricted <- round(sum(fit3$residuals^2)/(10^11),4)
rss_restricted

# ((RSS_R - RSS_U)/RSS_U)*((n-k-1)/r) this is a F(r,n-k-1)
# n sample size
# k : # regressors
# r : # restrictions
((rss_restricted - rss_unrestricted)/rss_unrestricted)*((507-4-1)/4)


qf(0.95, 4, 541) # CRITICAL VALUE


##################################
##### Answer to question 6 #######
##################################

confint(fit, level = 0.99)

?confint
##################################
##### Answer to question 7 #######
##################################

fit2 <- lm(saleprice ~ bedroom,hprice_free)
summary(fit2)


res=fit2$residuals
#Regression Hypothesis
#1)
mean(res)
#2) Homoschedastic?
plot(res)
white_test(fit2)
#3) Independence
x11()
par(mfrow=c(2,2))

plot( hprice_free$lotsize,res)
plot( hprice_free$bedroom,res)
plot( hprice_free$bath,res)
plot( hprice_free$stories,res)
dev.off()
#4) OK
#5) Gaussianity
x11()
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

jarque.bera.test(res)

##################################
##### Answer to question 8 #######
##################################

# Intercept: Considering a significance level of 5% and the p-value in model (2),
# we can reject the null hypothesis (H0 : Beta0 = 0) in favor of the alternative hypothesis
# (H1 : veta_0 != 0). 
# In model (1) we accepted the null hypothesis (H0 : beta_0 = 0).

# Regressor: Considering a significance level of 5% and the p-value of the coefficient associated with the explanatory variable (x2i) in model (2), we can reject the
# null hypothesis (H0 : beta_2 = 0) in favor of the alternative hypothesis (H1 : beta_2!= 0).
# Hence, beta_2 is statistically significant.
# In model (2), i.e. univariate regression model, houses with an extra bedroom
# will tend to be worth $12, 992 more than houses without the extra bedroom.
# In model (1), i.e. multiple regression model, houses with an extra bedroom
# will tend to be worth $2298 more than houses without the extra bedroom,
# ceteris paribus.
# We can notice that beta_2 is much higher in the univariate regression model 
# than in the multiple regression model. See the following topic: Omitted Variable Bias (and also multicollinearity).

# With this comparison and as highlighted in the assignment 3 (point 8), we want
# to highlight the impact of the omission of relevant variables to the regression. To
# state that houses with an extra bedroom tend to be worth $13, 270 more can be
# misleading given the fact that house price can be influenced by many factors and
# not only by the number of bedrooms.
# Furthermore, these factors can be correlated between them. Example: bigger
# houses tend to have more bedrooms, more bathrooms, more storeys and etc.

cor(hprice)
