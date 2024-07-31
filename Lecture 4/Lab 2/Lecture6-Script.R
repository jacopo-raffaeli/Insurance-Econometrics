install.packages("dyn")
install.packages("dynlm")
install.packages("orcutt")
library(dyn)
library(dynlm)
library(zoo)
library(lmtest)
library(orcutt)
library(sandwich)

### Line of code which checks if the package is already installed
if(!require(dyn)) install.packages("dyn")

?file.choose #choose a file interactively.
mydata <- read.table(file.choose(), sep = "\t", header=TRUE)

mytime <- zoo(mydata)

head(mytime, n = 3)

mkt_cap <- mytime[,1]
oil_pr <- mytime[,2]


##################################
##### Answer to question 1 #######
##################################

oil_pr_1 <- lag(oil_pr, -1, na.pad = TRUE)

?dynlm
fit <- dynlm(mkt_cap ~ oil_pr + oil_pr_1)

#### OR fit <- dyn$lm(mkt_cap ~ oil_pr + oil_pr_1)
summary(fit)

# Alternative method

fit <- dynlm(mkt_cap ~ oil_pr + L(oil_pr, 1))



##################################
##### Answer to question 2 #######
##################################

# Breusch-Pagan test
# H0: Homoskedasticity
bptest(fit , studentize = FALSE)

# White test
bptest(fit, ~ oil_pr + oil_pr_1 + I(oil_pr^2)+I(oil_pr_1^2)
       + I(oil_pr*oil_pr_1))

# The null hypothesis is homoskedasticity (no heteroskedasticity).
# Considering a significance level of 5% and the p-value of Breusch-Pagan test (or
# White test or both), we cannot reject the null hypothesis. Hence, we have 
# homoscedasticity.
# Notice that p-value > 0.05.


##################################
##### Answer to question 3 #######
##################################

# Durbin Watson test

?dwtest
dwtest(fit)

# Breusch Godfrey test
?bgtest

# H0: there is no autocorrelation.
# Alternative statistic: TR^2 ~ Chi-square
bgtest(fit, order = 6)

# Box-Ljung test and Box-Pierce test
Box.test(fit$residuals, type = "Ljung-Box", lag = 6)

Box.test(fit$residuals, type = "Box-Pierce", lag = 6)

acf(fit$residuals)

# Considering a significance level of 10% and the p-value of the first three tests
# (p-value < 0.1), we can reject the null hypothesis (H0) of no autocorrelation.
# Hence, we have autocorrelation.
# In the last test, we cannot reject the null hypothesis because the p-value of 
# Box-Pierce (0.102) > 0.1.


##################################
##### Answer to question 4 #######
##################################

oil_pr_2 <- lag(oil_pr, -2, na.pad = TRUE)
oil_pr_3 <- lag(oil_pr, -3, na.pad = TRUE)
oil_pr_4 <- lag(oil_pr, -4, na.pad = TRUE)

fit2 <- dynlm(mkt_cap ~ oil_pr + oil_pr_1 + oil_pr_2 + oil_pr_3 + oil_pr_4)

#OR fit2 <- dyn$lm(mkt_cap ~ oil_pr + oil_pr_1 + oil_pr_2 + oil_pr_3 + oil_pr_4)

summary(fit2)


# Alternative method
fitt<-dynlm(mkt_cap~ oil_pr+L(oil_pr, 1)+L(oil_pr, 2)+L(oil_pr, 3)+L(oil_pr, 4))
summary(fitt)

##################################
##### Answer to question 5 #######
##################################

# Durbin Watson test

dwtest(fit2)

# Breusch-Godfrey test:
# H0:is no autocorrelation.

# Alternative statistic: TR^2 ~ Chi-square
bgtest(fit2, order = 6)

# Box-Ljung test and Box-Pierce test

Box.test(fit2$residuals, type = "Ljung-Box", lag = 6)
acf(fit2$residuals)

# Considering a significance level of 10% and the p-value of the four tests 
# (p-value > 0.1), we cannot reject the null hypothesis (H0) of no autocorrelation.
# Hence, we cannot say there is not autocorrelation.


##################################
########   HAC STD ERR   #########
##################################

# HAC Standard Errors

?vcovHAC
coeftest(fit, vcov. = vcovHAC)
summary(fit)
coeftest(fit2, vcov. = vcovHAC)


##################################
########   NEWEY WEST   ##########
##################################

# Newey-West

?NeweyWest
# A set of functions implementing the Newey & West (1987, 1994) heteroskedasticity 
# and autocorrelation consistent (HAC) covariance matrix estimators.

NeweyWest(fit)
NeweyWest(fit1)



##################################
#######   COCHRANE ORCUTT   ######
##################################

# Interactive method using to solve first order autocorrelation problems. 
# This procedure estimates both autocorrelation and beta coefficients recursively 
# until we reach the convergence (8th decimal). The residuals are
# computed after estimating Beta using EGLS approach and Rho is estimated using 
# the previous residuals.

?cochrane.orcutt

fit <- dynlm(Market.capitalization ~ Oil.price.above.benchmark.price, 
             data = mydata)
summary(cochrane.orcutt(fit, convergence = 8, max.iter=100))


