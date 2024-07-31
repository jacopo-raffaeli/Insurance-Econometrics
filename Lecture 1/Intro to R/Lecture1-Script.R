rm(list = ls()) # For cleaning objects workspace

install.packages("foreign")
install.packages("xts")
install.packages("ggplot2")
install.packages('pastecs')
install.packages("psych")
install.packages("MASS")

#  Load packages
library(foreign) #Or you can use require(foreign)
library(xts)
library(ggplot2)
library(pastecs)
require(psych)
library(MASS)

# This function shows the current working directory
getwd()

# This function is used to set the working directory
# Notice that you must use slash NOT backslash
#setwd(""C:/Users/miche/OneDrive - Politecnico di Milano/INS&ECON/Econometrics/ECONOMETRIA 2022/LAB1/SCRIPT AND DATA")



# This function opens the data set which is a dta file (Stata)
mydata <- read.dta("djia_daily.dta")

# # Other method:
# mydata_1<- read.dta("C:/..../djia_daily.dta")
# data <- file.choose()
# mydata_2 <- read.dta(data)

##################################
##### Answer to question 1 #######
##################################

# The head function shows the first five rows of our set of data
head(mydata, n = 5)

# Other useful functions

# The head function shows the first n rows of our set of data
head(mydata, n = 10) 

# The tail function shows the last six rows of our set of data
tail(mydata) 

######

# View shows the tabular version of data
View(mydata)

# Class shows the class of the variable
class(mydata)

# This function displays the structure of the object
str(mydata)

# This function shows the dimension of the object
dim(mydata)

# Plot DJIA with daily values

# xts is a constructor function for creating an extensible time-series object.
? xts
dow <- xts(mydata[,4], mydata[,5])
plot(dow, col = "orange", main = "Dow Jones Industrial Average Index")

##################################
##### Answer to question 2 #######
##################################

# How to identify elements inside a matrix of data?
# Some examples:
# We select the second row
mydata[2,]

# We select the second row and fourth column
mydata[2,4]

# Rows: 2,3,4,5 Columns:4,5
mydata[2:5,4:5]

? length
length(mydata[,4])

# We calculate the returns by means of the log difference
mydata[2:length(mydata[,4]),4] <- 100*diff(log(mydata[,4]))

# diff returns suitably lagged and iterated differences (defualt lag = 1)

# We insert a NA value in the first row of the fourth column
# Shifting from prices to returns, we lose the first observation

mydata[1,4]=NA
head(mydata, n=10)

# We select only the 4th and 5th column
# corresponding to dates and returns

ret_djia <- xts(mydata[, 4], mydata[, 5])
dim(ret_djia)
View(ret_djia)

# main is the title of the plot
plot(ret_djia, main = "Daily returns of DJIA", col = 'red', cex.axis = 0.9)

# Save the plot in the directory
png(file = "plot_ret.png", bg = "transparent")
plot(ret_djia, main = "Daily returns of DJIA")
dev.off() #close the operation

##################################
##### Answer to question 3 #######
##################################

library(pastecs)
stat.desc(ret_djia)

# nbr == number of values
# coef.var is defined as the standard deviation divided by the mean

# # Another way:
# stat.desc(mydata$djia)
# # Mean
# mean(mydata$djia, na.rm = TRUE)
# 
# # Variance
# var(mydata$djia, na.rm = TRUE)
# 
# # Standard deviation
# sd(mydata$djia, na.rm = TRUE)
# 
# # na.rm = TRUE indicates the NA values are not considered
# 
# summary(ret_djia)
# 
# # Other statistics
# min(na.omit(rdjia))
# 
# max(na.omit(rdjia))
# 
# median(na.omit(rdjia))
# 
# quantile(na.omit(rdjia))
# 
# quantile(na.omit(rdjia), c(0.25, 0.5, 0.75))

# Other packages and examples
install.packages("psych")
require(psych)

# Kurtosis
kurtosi(mydata$djia, na.rm = TRUE)

# Skewness
skew(mydata$djia, na.rm = TRUE)

describe(mydata$djia)
install.packages("Hmisc")

require(Hmisc) # This install package Hmisc
describe(mydata$djia)

##################################
##### Answer to question 4 #######
##################################

ret_djia_sub <- ret_djia["1987-09-01/1987-10-17"]
dim(ret_djia_sub)

x11()
plot(ret_djia_sub) #Graph of returns in the subperiod 1987:09:01-1987:10:17
png(file = "plot_subsample_return.png", bg = "transparent")
plot(ret_djia_sub, main = "Daily returns of DJIA - subsample")
dev.off()

mean(ret_djia_sub)

var(ret_djia_sub)

sd(ret_djia_sub)

# Boxplot

summary(ret_djia_sub)

quantile(coredata(ret_djia_sub), c(0.25, 0.5, 0.75))
x11()
boxplot(coredata(ret_djia_sub))
abline(h = min(coredata(ret_djia_sub)), col = "Blue")
abline(h = max(coredata(ret_djia_sub)), col = "Yellow")
abline(h = median(coredata(ret_djia_sub)), col = "Green")
abline(h = quantile(coredata(ret_djia_sub), c(0.25, 0.75)), col = "Red")


##################################
##### Answer to question 5 #######
##################################

# The density function creates the density
d_ret <- density(ret_djia_sub)

plot(d_ret, main="Density")

# Save the plot in the directory
png(file = "plot_density.png")
plot(d_ret, main="Density")
dev.off()


##################################
##### Answer to question 6 #######
##################################

? pnorm
pnorm(-22,mean=mean(ret_djia_sub),sd=sd(ret_djia_sub))

pnorm(-2, mean=mean(ret_djia_sub),sd=sd(ret_djia_sub))


##################################
##### Answer to question 7 #######
##################################

# We have to use pt function
?pt

## P[X <= x]
pt(-2, df= 3, lower= TRUE)

## P[X > x]
pt(-2, df= 3, lower= FALSE)

1 - pt(-2, df= 3, lower= FALSE)


##################################
##### Answer to question 8 #######
##################################

# We have to use rnorm function. See help
# rnorm generates random deviates.

?rnorm
?set.seed

set.seed(123)

dgauss <- rnorm(length(ret_djia_sub), mean = 5, sd = 2)
length(dgauss)


##################################
##### Answer to question 9 #######
##################################

plot(dgauss, ret_djia_sub, main="Returns and Normal Variable",
     xlab="dgauss", ylab="ret", col = "blue")
abline(h= 0, col = "red", lty=2)

png(file = "scatter_ret_gaus.png")
plot(dgauss, ret_djia_sub, main="Returns and Normal Variable",
     xlab="dgauss ", ylab="ret ",col = "blue")
dev.off()

# Other considerations

# Correlation
cor(dgauss, ret_djia_sub)

# Covariance
cov(dgauss, ret_djia_sub)

# Correlation
cov(dgauss, ret_djia_sub)/(sd(dgauss)*sd(ret_djia_sub))

cor(dgauss, ret_djia_sub)/sqrt((1-(cor(dgauss, ret_djia_sub)^2))/(length(ret_djia_sub) - 2))

# Correlation test
# H0: rho = 0
# H1: rho != 0
cor.test(dgauss, ret_djia_sub) 

# p-value = 0.6307 -> we cannot reject (see also CI)


#### BONUS

library(MASS)
pos <- mvrnorm(100, mu = c(0, 0),
               Sigma = matrix(c(1, 2, 8, 8), ncol = 2), empirical = TRUE)
cor.test(pos[,1], pos[,2])
neg <- mvrnorm(100, mu = c(0, 0),
               Sigma = matrix(c(1, -2, -8, 8), ncol = 2), empirical = TRUE)
cor.test(neg[,1], neg[,2])
nocorr <- mvrnorm(100, mu = c(0, 0),
                  Sigma = matrix(c(1, 0, 0, 1), ncol = 2), empirical = TRUE)
cor.test(nocorr[,1], nocorr[,2])

x11()

par(mfrow = c(1,2))
plot(pos, lwd = 2, col = 'orange', xlab = 'x', ylab = 'y',
     main = "Correlation = 0.71")
plot(neg, lwd = 2, col = 'blue', xlab = 'x', ylab = 'y',
     main = "Correlation = - 0.71")
dev.off()
plot(nocorr, lwd = 2, col = 'purple', xlab = 'x', ylab = 'y',
     main = "Correlation = 0")



##################################
##### Answer to question 10 ######
##################################

?write.csv
write.csv(as.data.frame(ret_djia),"daily_ret_djia.csv")
# Another way (in txt file)
write(rdjia,"daily_ret_djia.txt")

