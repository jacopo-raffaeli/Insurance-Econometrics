rm(list=ls())
library(lmtest)
library(dynlm)
library(orcutt)
library(sandwich)
library(xts)
library(urca)
library(quantmod)
library(fUnitRoots)
library(FinTS)
library(rugarch)
library(strucchange)
library(ggplot2)
library(forecast)
library(alfred)
library(readxl)
library(whitestrap)
library(devtools)
library(tseries)
library(tidyverse)
library(base)

mydata <- read_excel('Italy cds zspread monthly.xlsx')
mydata <- read_excel(file.choose())


price_cds_ToT <- ts(mydata[,"CDS"])
Z_spread_ToT <- ts(mydata[,"Z-spread"])

cds = price_cds_ToT[1:171,] 
spread = Z_spread_ToT[1:171,]    

##########################################
######## Plot cds and spread #############
##########################################

# spreads PLOT

par(mfrow=c(1,1))
plot(cds, xlab="Indexed time", ylab="Basis Points", type='l', main="CDS vs. Z-spread\nITALY", col="red", ylim = c(20,700))
lines(spread, col="blue")
legend(120,550,legend=c('Z-spread','CDS'),col=c('blue','red'),lty=1:1,cex=0.8)


################################################################################
######## Linear regression #####################################################
################################################################################

fit <- lm(spread ~ cds)
fit <- lm(cds ~ spread )
summary(fit)
res=fit$residuals 

#1)
mean(res) # spread: 1.361727e-15 cds: -2.940306e-16.

#2) Homoschedastic?

plot(res,  xlab="Indexed time", ylab="Basis Points", main="Residuals - homoscedasticity", col="blue")
library(whitestrap)
white_test(fit) 
# P-value: 0, not Homoskedasticity for zspread.
# P-value: 2e-06, not Homoskedasticity for cds.

#3) Independence

plot(res[1:length(res)-1],res[2:length(res)], xlab="u_t", ylab="u_t-1", main="Residuals - autocorrelation", col="blue")
library(lmtest)

# Durbin Watson
dwtest(fit) 
# zspread: p-value < 2.2e-16 alternative hypothesis: true autocorrelation is greater than 0.
# cds: p-value < 2.2e-16 alternative hypothesis: true autocorrelation is greater than 0.

#4) OK is not random, we have the data

#5) Gaussianity

qqnorm(res)
qqline(res) 

library(tseries)
jarque.bera.test(res) 
# p-value = 8.61e-07 for zspread.
# p-value = 8.296e-08 for cds.


####################################
######## UNIT ROOT TEST ############
####################################

# ERS unit root test
# H0: Non-stationary
# H1: stationary

library(urca)

library(aTSA)
# Augmented-Dickey Fuller test
# H0: non-stationarity
adf.test(cds, output = TRUE) # Accept non stationarity
adf.test(spread, output = TRUE) # Accept non stationarity

# Elliot, Rothenberg and Stock (ADF-GLS test)
# Tests stationarity
# H0: non-stationarity
cds.urers1 <- ur.ers(cds, type="P-test", model="trend")
summary(cds.urers1) # Cannot reject H0 7.8466 > 6.86.
spread.urers2 <- ur.ers(spread, type="P-test", model="trend")
summary(spread.urers2) # Cannot reject H0 10.0104  > 6.86.

# we have no stationarity.
# we have to take the differences of the two time series and check the unit root.

dcds <- diff(cds)
dspread <- diff(spread)

# create time series 
dcds <- ts(dcds)
dspread <-ts(dspread) 

# Augmented-Dickey Fuller test
adf.test(dcds, output = TRUE) # Reject non stationarity
adf.test(dspread, output = TRUE) # Reject non stationarity

dcds.urers1 <- ur.ers(dcds, type="P-test", model="trend")
summary(dcds.urers1) # Reject H0 0.0672 < 4.05. 

dspread.urers2 <- ur.ers(dspread, type="P-test", model="trend")
summary(dspread.urers2) # Reject H0 0.1406 < 4.05.

# so I can reject H0 for the differences.

##########################################
######## PLOT DIFF cds and zspread #######
##########################################

# Plot diff - zspread and cds

par(mfrow=c(1,1))
plot(dcds, type='l', xlab = "Indexed time", ylab="Basis Points", main="(difference)CDS vs. (difference)Z-Spread\nITALY", col="blue", ylim=c(-150,220))
lines(dspread, col="red")


#######################################
######## Breaking Points ##############
#######################################

?Fstats

fs.y_cds <- Fstats(dcds ~ 1) # Find possible breakpoint
plot(fs.y_cds)
# No breakpoint since threshold is not reached

fs.y_zsp <- Fstats(dspread ~ 1) # Find possible breakpoint
plot(fs.y_zsp)


plot(dcds)
lines(breakpoints(fs.y_cds))

plot(dspread)
lines(breakpoints(fs.y_zsp))


#################################################
######## VAR MODEL  (no error correction) #######
#################################################

library(vars)
y <- cbind(dcds, dspread)
colnames(y) <- c("dcds","dspread")
y <- na.trim(y)

# determine number of lags to be included in cointegration 
# test and in VEC model (AIC)
# The function "VARselect" returns information criteria and 
# final prediction error for sequential increasing 
# the lag order up to a VAR(p)-process, which are based 
# on the same sample size.

y.VAR.IC <- VARselect(y, type="const")
nlags <- y.VAR.IC$selection["SC(n)"]
nlags 

fit_cds <- dynlm(dcds ~ L(dcds,1)+ L(dcds,2)+ L(dcds,3)+ L(dspread,1)+ 
                   L(dspread,2)+ L(dspread,3))
fit_lm <- dynlm(dspread ~ L(dcds,1)+ L(dcds,2)+ L(dcds,3)+ L(dspread,1)+
                  L(dspread,2)+ L(dspread,3))
summary(fit_cds)
summary(fit_lm)

# Load package
library(vars)

# Estimate model again with this function (easier)
model <- VAR(cbind(dspread,dcds), p = 3, type = "const")
summary(model)
# Correlations between residuals are high, therefore
# I have to orthogonalise the matrix for the IRF

##############################################
############Impulse Response Function#########
##############################################

# Dspread leading
model <- VAR(cbind(dspread,dcds), p = 3, type = "const")
summary(model)

feir <- irf(model, impulse = "dcds", response = "dspread",
            n.ahead = 8, ortho = TRUE, runs = 1000)
plot(feir)
# Positive response and significant (sign consistent with expectations)
# Cds is shocked with a lag because spread is the leading variable
# In matrix terms, spread is the first component of the orthogonalized matrix
# [u_1; = [Pi_11    0;     *   [epsilon_1;
#  u_2]    Pi_21 Pi_22]         epsilon_2]


feir <- irf(model, impulse = "dspread", response = "dcds",
            n.ahead = 8, ortho = TRUE, runs = 1000)
plot(feir)

#Negative response and not significant (sign non consistent with expectations due to noise in CDS)
# Spread is shocked and is instantaneously affecting also cds

# If you do not orthogonalise the response is not simultaneous

feir <- irf(model, impulse = "dcds", response = "dspread",
            n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir)

feir <- irf(model, impulse = "dspread", response = "dcds",
            n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feir)


# Case with dCDS leading

# Estimate model again with this function
model <- VAR(cbind(dcds, dspread), p = 3, type = "const")
summary(model)

feir <- irf(model, impulse = "dcds", response = "dspread",
            n.ahead = 8, ortho = TRUE, runs = 1000)
plot(feir)

feir <- irf(model, impulse = "dspread", response = "dcds",
            n.ahead = 8, ortho = TRUE, runs = 1000)
plot(feir)

#################################
#####Granger  Causality##########
#################################

causality(model, cause = "dcds", vcov.=NULL, boot=FALSE, boot.runs=100)
causality(model, cause = "dspread", vcov.=NULL, boot=FALSE, boot.runs=100)

#Granger Causality: does the past of variable x help improve the prediction of future values of y? (again more noise in CDS).
#Instantaneous Causality:  does knowing the future of x help me better predict the future of y? 


#########################################
######## COINTEGRATION ##################
#########################################

# Perform cointegration test: Johansen test

library(urca)
y.CA <- ca.jo(cbind(spread, cds), type="trace", ecdet = "none",K=3)
summary(y.CA)
# nlags = 3:
# 27.67 > 23.52 --> we reject r=0 (in the lecture notes k=0 - no cointegration)
# 4.61 < 11.65 --> we accept r<=1 (cointegration of order 1)

# Add constant in the cointegration
y.CA_const <- ca.jo(cbind(spread, cds), type="trace", ecdet = "const",K=3) 
summary(y.CA_const)

# We select the most simple model (with no constant) for sake of parsimony in 
# the cointegration

# Compute cointegration relationship Spread on cds
fit <- lm(spread ~ cds -1 )
summary(fit)
res=fit$residuals 
x = spread+cds*y.CA@V[2,1] #cointegration relationship
fit <- lm(res ~ x-1)
summary(fit)
# Notice that residuals of the OLS model are very similar to the
# ones of the Johansen linear combination (eigenvectors)

par(mfrow=c(1,1))
plot(ts(x), type='l', xlab = "Indexed time", ylab="Basis Points", main="Residuals", col="blue", ylim=c(-150,220))
lines(ts(res), col="red")

# Notice beta is equal to the value of the eigenvector (second value)

# Compute VEC model of lag 1
fit <- lm(dspread ~ x[1:170])
summary(fit)

# Compute VEC model of lag 1
fit <- lm(dcds ~ I(-x[1:170]))
summary(fit)

# We notice that the Error correction model on ZSpread is significant, while it is
# not true for Cds. This means that we are in a cointegration framework, but
# the CDS is less liquid and therefore there is more noise in the differences.

###########################################
######## UNRESTRICTED VEC MODEL ###########
###########################################

# Here we can expand the VEC for more lags.
# Estimate VEC model

y.VEC <- cajorls(y.CA, r=1) # r is the number of cointegration

# to see t-statistics and p-values
summary(y.VEC$rlm)
y.VEC$rlm$residuals
# hyp 1: Zero mean of the residuals
mean(y.VEC$rlm$residuals[,1])
mean(y.VEC$rlm$residuals[,2])

# hyp 2: cov matrix psd
cor(y.VEC$rlm$residuals)
# If you want to check if the matrix is positive semi-definite, uncomment these lines
# install.packages("matrixcalc")
# library(matrixcalc)
# is.positive.semi.definite(cov(y.VEC$rlm$residuals), tol=1e-8)

# hyp 3: on autocorrelation
dwtest(residuals(vec2var(y.CA, r=1))[,1] ~ residuals(vec2var(y.CA, r=1))[,2])
# p-value = 0.5604s

