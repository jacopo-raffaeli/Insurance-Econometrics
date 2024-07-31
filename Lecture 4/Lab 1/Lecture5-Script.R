install.packages("lmtest")
install.packages("ggplot2")
install.packages("sandwich")
library(lmtest)
library(ggplot2)
library(sandwich)

### Line of code which checks if the package is already installed
if(!require(lmtest)) install.packages("lmtest")

# Import data

?file.choose #choose a file interactively.
mydata <- read.csv(file.choose(), sep=" ", header=T)

?attach

# objects in the database can be accessed by simply giving their names.
attach(mydata)
summary(mydata)


##################################
##### Answer to question 1 #######
##################################

ols_model <- lm(food_exp ~ income)
summary(ols_model)
       
# Plot errors

resi <- ols_model$residuals
library(ggplot2)
ggplot(data = mydata, aes(y = resi, x = income)) + geom_point(col = 'blue') +
  geom_abline(slope = 0)

# White test: notice that you must setup the terms in the
# auxiliary model yourself
# H0: Homoskedasticity

bptest(ols_model,~ income + I(income^2)) 

# I() function to perform arithmetic operations inside a formula.

# Manually:
u_2 <- summary(ols_model)$residuals^2
R_2 <- summary(lm(u_2 ~ income + I(income^2)))$r.squared
stats <- nrow(mydata)*R_2
stats

df = 2
# p-value
pchisq(stats, df, lower.tail = FALSE)

# In alternative you could use the Breusch-Pagan test
# H0: Homoskedasticity

bptest(ols_model,~income, studentize = FALSE)


##################################
##### Answer to question 2 #######
##################################

nyw = food_exp/sqrt(income) # weighted y
nxw = income/sqrt(income) # weighted x
ncw = 1/sqrt(income) # weighted constant
fit_WLS1 <- lm(nyw ~ nxw + ncw-1) # remove original constant
summary(fit_WLS1)

# White test

bptest(fit_WLS1,~ nxw + ncw + I(nxw^2)+ I(ncw^2) + I(nxw*ncw))

resi_2 <- fit_WLS1$residuals
library(ggplot2)
ggplot(data = mydata, aes(y = resi_2, x = income)) + geom_point(col = 'blue') +
  geom_abline(slope = 0)

##################################
##### Answer to question 3 #######
##################################


nyw = food_exp/sqrt(sqrt(income)) # weighted y
nxw = income/sqrt(sqrt(income)) # weighted x
ncw = 1/sqrt(sqrt(income)) # weighted constant
fit_WLS2 <- lm(nyw~nxw+ncw-1) # remove original constant

summary(fit_WLS2)

# White test

bptest(fit_WLS2,~ nxw + ncw + I(nxw^2)+ I(ncw^2) + I(nxw*ncw))


##################################
##### Answer to question 4 #######
##################################
        
nyw = food_exp/sqrt(income^2) # weighted y
nxw = income/sqrt(income^2) # weighted x
ncw = 1/sqrt(income^2) # weighted constant
fit_WLS3 <- lm(nyw~nxw+ncw-1) # remove original constant

summary(fit_WLS3)

# White test

bptest(fit_WLS3,~ nxw + ncw + I(nxw^2)+ I(ncw^2) + I(nxw*ncw))


##################################
##### Answer to question 5 #######
##################################

nyw = food_exp/sqrt(log(income)) # weighted y
nxw = income/sqrt(log(income)) # weighted x
ncw = 1/sqrt(log(income)) # weighted constant
fit_WLS4 <- lm(nyw~nxw+ncw-1) # remove original constant

summary(fit_WLS4)

# White test

bptest(fit_WLS4,~ nxw + ncw + I(nxw^2)+ I(ncw^2) + I(nxw*ncw))


##################################
##### Answer to question 6 #######
##################################

# 1) estimate OLS model
fit_OLS <- lm(food_exp~income)

# 2) store residuals and take log
res <- resid(fit_OLS)
log_res_sq<-log(res^2)

# 3) take logs
log_income <- log(income)

# 4) Fit residuals with linear model and store fitted values
fit_log <-lm(log_res_sq ~ log_income)
fit_val<-fitted(fit_log)

# 5) use fitted values as weights for WLS
nyw <- food_exp/sqrt(exp(fit_val))
nxw <- income /sqrt(exp(fit_val))
ncw <- 1/sqrt(exp(fit_val))

# 6) fit new model
fit_wls <-lm(nyw ~ nxw + ncw -1)
summary(fit_wls)

# or

fit_log <- lm(log_res_sq ~ log_income)
varfunc <- exp(fit_val)
fit_wls2 <- lm(food_exp ~ income, weights = 1/sqrt(varfunc))
summary(fit_wls2)

# White test

bptest(fit_wls,~ nxw + ncw + I(nxw^2)+ I(ncw^2) + I(nxw*ncw))

# Breusch-Pagan test

bptest(fit_wls,~nxw+ncw, studentize = FALSE)

# Plot

library(ggplot2)
g <- ggplot(data = mydata, aes(y = food_exp, x = income)) +
  geom_point(col = 'blue')
g + geom_abline(slope = ols_model$coefficients[2], 
                intercept = ols_model$coefficients[1], col = 'red') + 
  geom_abline(slope = fit_wls2$coefficients[2], 
              intercept = fit_wls2$coefficients[1], col = 'green')

##################################
######  Robust Std errors  #######
##################################

# Heteroskedastic Consistent Standard Errors (HCE)
food.ols <- lm(food_exp ~ income)
summary(food.ols)

library(lmtest)
library(sandwich)
coeftest(food.ols, vcov = vcovHC(food.ols, "HC1"))   
# HC1 gives us the White standard errors

# The huge difference in standard errors is probably due to our small sample size.
# Regressing with robust standard errors addresses the issue of computing 
# incorrect interval estimates or incorrect values for our test statistics. 
# However, it doesn't address the issue of the second consequence of heteroskedasticity,
# which is the least squares estimators no longer being best.

# In general, one can combine WLS and HCE:
# 1. Correct heterosk. with WLS
# 2. If residuals are homosk. -> STOP
# 3. If residuals are heterosk. -> use HCE on the WLS residuals




