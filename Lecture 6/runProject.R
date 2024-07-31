
rm(list = ls())                                                                         # Clear workspace
graphics.off()                                                                          # Clear plots
cat("\014")                                                                             # Clear command window                                                                          # Setting the seed
# setwd("/Users/macbook/Desktop/ingegneria matematica/MAGISTRALE/SECONDO ANNO /Secondo semestre/Econometrics/Final_Project/Code")       # Set directory

# install.packages("nortest") # To use Anderson-Darling test
# install.packages("tseries") # To use Augmented Dickey-Fuller test
# install.packages("rugarch") # Used in modelling
# install.packages("fGarch") # Used in modelling
# install.packages("FinTS") # Used in ARCH test
# install.packages("dynlm") # To add lags in the model
# install.packages("vars") # To use VAR
# install.packages("nlWaldTest") # To use non-linear Wald test
# install.packages("lmtest") # Used in BP test
# install.packages("broom")
# install.packages("car")
# install.packages("sandwich")
# install.packages("knitr")
# install.packages("forecast")
# install.packages("tsbox")
# install.packages("stats")
# install.packages("zoo")
# install.packages("vrtest")
# install.packages("strucchange")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("changepoint")
# install.packages("stats")
# install.packages("moments")
# install.packages('nortest')
library(nortest)
library(DescTools)
library(readr)                                                                          # To read CSV files
library(moments)                                                                        # To use statistical functions
library(nortest)                                                                        # To use Anderson-Darling test
library(ggplot2)                                                                        # To use ggplot
library(tseries)                                                                        # To use Augmented Dickey-Fuller test
library(rugarch)                                                                        # Used in modelling
library(fGarch)                                                                         # Used in modelling
library(FinTS)                                                                          # Used in ARCH test
library(dynlm)                                                                          # To add lags in the model
library(vars)                                                                           # To use VAR
library(nlWaldTest)                                                                     # To use non-linear Wald test
library(lmtest)                                                                         # Used in BP test
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(tsbox)
library(stats)
library(zoo)
library(vrtest)
library(FinTS)
library(strucchange)
library(tidyverse)
library(lubridate)
library(changepoint)                                                                    # Used for structural breaks detection
library(stats)
library(moments)
library("dgof")

# RUNNING TIME ~ 50 seconds

##############################
### Analysis last 15 years ###
##############################

data   <- read_csv(file.choose()) #DAX                                                           # Import the CSV file (Daily data)
data   <- data[data[,2]!="null",]  
data=data[3548:3804,]
# Remove null rows
Dates  <- as.Date(data$Date);                                                           # Saving dates
Prices <- as.numeric(data$`Adj Close`);                                                 # Saving prices

N <- length(Prices);                                                                    # Number of observations
X <- log(Prices[2:N]/Prices[1:N-1])                                                     # Log-returns computation
wins=1
if (wins==1)
  {X=Winsorize(X, minval = NULL, maxval = NULL, probs = c(0.01, 0.99),
          na.rm = FALSE, type = 7)}


# PLOTS
plot(Dates, Prices, type = "l", xlab = "Date", ylab = "Price")                          # Prices plot
plot(Dates[2:N], X, type = "l", xlab = "Date", ylab = "Log-Returns")                    # Log-returns plot
# It is not stationary because it has non-constant variance 
                                            # Ignore first date ( length(X) = length(Prices)-1 )

# Descriptive statistics
Summary  <- summary(X);                                                                 # Summary of the data
Std_Dev  <- sd(X);                                                                      # Standard deviation
Skewness <- skewness(X);                                                                # Skewness (Gaussian = 0)
Kurtosis <- kurtosis(X);                                                                # Kurtosis (Gaussian = 3)


# Gaussianity tests
shapiro_test <- shapiro.test(X)                                                         # Shapiro-Wilk test
ad_test      <- ad.test(X)                                                              # Anderson-Darling test
ks_test      <- ks.test(X, "pnorm", mean = mean(X), sd = sd(X))                         # Kolmogorov-Smirnov test
jb_test      <- jarque.bera.test(X)                                                     # Jarque-Bera test

# Display
cat("Shapiro-Wilk test:",       shapiro_test$p.value, "\n")                             # p-value of Shapiro-Wilk test
cat("Anderson-Darling test:",   ad_test$p.value,      "\n")                             # p-value of Anderson-Darling test
cat("Kolmogorov-Smirnov test:", ks_test$p.value,      "\n")                             # p-value of Kolmogorov-Smirnov test
cat("Jarque Berà test:",        jb_test$p.value,      "\n")                             # p-value of Jarque-Bera test

# QQ-plot for x
qqnorm(X)                                                                               # QQ-plot of log-returns
qqline(X)                                                                               # QQ-line which represents gaussianity

font_name <- "Times"
font_size <- 20

# Plot versus a gaussian with same mean and standard deviation
df <- data.frame(x = X)                                                                 # Create a data frame for the density plot
ggplot(df, aes(x = x)) +                                                                # Create a plot object
  geom_density(fill = "blue", alpha = 0.2) +                                            # Plot the density of X
  stat_function(fun = dnorm, args = list(mean = mean(X), 
                                         sd = sd(X)), color = "red") +                  # Theoretical normal distribution
  labs(title = "Log-Return Distrubution", x = "", y = "", 
       color = "Distribution") +                                                        # Add a legend
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical"))    +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.20),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
        axis.line = element_line(color = "lightgrey"))                        # Set colors



# Stationarity (unit root perspective)
adf_test <- adf.test(X)                                                                 # Augmented Dickey-Fuller test
cat("Augmented Dickey-Fuller test:", adf_test$p.value,     "\n")                        # Displaying results
# We reject the null hypothesis => Stationary (from a unit root prospective)

# Non-null average test
zm_test <- t.test(X, mu = 0);                                                           # t-test for zero mean
cat("Zero mean test:",zm_test$p.value,"\n")                                             # p-value = 0.4942 => Zero mean


# ACF and PACF
acf.X  <- acf( X, main = "ACF ", lag.max = 30)                                          # ACF plot of the log-returns
pacf.X <- pacf(X, main = "PACF", lag.max = 30)                                          # Partial ACF plot of the log-returns

# ACF and PACF
acf.X  <- acf( X^2, main = "ACF", lag.max = 30)                                         # ACF plot of the squared log-returns
pacf.X <- pacf(X^2, main = "PACF", lag.max = 30)                                        # Partial ACF plot of the squared log-returns

# ARCH effect test
archTest <- ArchTest(X, lags = 1, demean = TRUE)                                        # ARCH test on log-returns                                     
archTest                                                                                # Display results of the ARCH test                                                  
# Accept H0 --> No ARCH effect

archTest <- ArchTest(X, lags = 2, demean = TRUE)                                        # ARCH test on log-returns                                     
archTest                                                                                # Display results of the ARCH test                                                  
# Accept H0 --> No  ARCH effect

archTest <- ArchTest(X, lags = 3, demean = TRUE)                                        # ARCH test on log-returns                                     
archTest                                                                                # Display results of the ARCH test                                                  
# Reject H0 --> There is ARCH effect


font_name <- "Times"
font_size <- 16


#############################################
### Model fitting & residuals diagnostics ###
#############################################

### GARCH(1,1) ###
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),                          # GARCH(1,1) specifications
                        variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),   
                        distribution.model = 'norm');
sgarch.fit = ugarchfit(data = as.array(X), spec = spec);                                # GARCH(1,1) fitting 
sgarch.fit

# Diagnostic 
resid <- residuals(sgarch.fit)                                                          # Extract the residuals
std_resid <-  resid / sigma(sgarch.fit)                                                 # Standardized 
mean(std_resid)                                                                         # Mean
var(std_resid)                                                                          # Variance 
skewness(std_resid)                                                                     # Skewness
kurtosis(std_resid)                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)
qqline(std_resid) 

# Distribution plot vs standard normal (same procedure as above for the distribution plot)
df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "GARCH(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical") )  +
                       theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
                             axis.title = element_text(family = font_name, size = font_size),
                             panel.background = element_rect(fill = "white"),
                             panel.grid.major = element_line(color = "lightgrey", size = 0.20),
                             panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
                             axis.line = element_line(color = "lightgrey"))                        # Set colors
  

# Ljung-Box test 
Box.test(std_resid, type = "Ljung-Box")                                                 # Ljung-Box test
# accept H0 --> there is not residual autocorrelation 


### GARCH-M(1,1) ### (GARCH in Mean, to find the presence of a risk premium)
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0),archm=TRUE,archpow=1),     # GARCH-M(1,1) specifications
                        variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                        distribution.model = 'norm')
sgarchM.fit = ugarchfit(data = as.array(X), spec = spec)                                # GARCH-M(1,1) fitting
sgarchM.fit

# Diagnostic 
resid     <- residuals(sgarchM.fit)                                                     # Extract the residuals
std_resid <-  resid / sigma(sgarchM.fit)                                                # Standardized 
mean(std_resid)                                                                         # Mean
var(std_resid)                                                                          # Variance 
skewness(std_resid)                                                                     # Skewness
kurtosis(std_resid)                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)                                                                              
qqline(std_resid) 

# Distribution plot vs standard normal (same procedure as above for the distribution plot)
df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "GARCH-M(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical") )  +
                       theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
                             axis.title = element_text(family = font_name, size = font_size),
                             panel.background = element_rect(fill = "white"),
                             panel.grid.major = element_line(color = "lightgrey", size = 0.20),
                             panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
                             axis.line = element_line(color = "lightgrey"))                        # Set colors
  

# Ljung-Box test 
Box.test(std_resid, type = "Ljung-Box")                                                 # Ljung-Box test
# accept H0 --> there is not residual autocorrelation 

### eGARCH(1,1) ###
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),                          # eGARCH(1,1) specifications
                        variance.model = list(model = 'eGARCH', garchOrder = c(1,1)),
                        distribution.model = 'norm')
egarch.fit = ugarchfit(data = as.array(X), spec = spec)                                 # eGARCH(1,1) fitting
egarch.fit

# Diagnostic 
resid     <- residuals(egarch.fit)                                                       # Extract the residuals
std_resid <-  resid / sigma(egarch.fit)                                                  # Standardized 
mean(std_resid);                                                                         # Mean
var(std_resid);                                                                          # Variance 
skewness(std_resid);                                                                     # Skewness
kurtosis(std_resid);                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)                                                                              
qqline(std_resid) 

# Distribution plot vs standard normal (same procedure as above for the distribution plot)
df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "eGARCH(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical"))  +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.20),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
        axis.line = element_line(color = "lightgrey"))                        # Set colors


# Ljung-Box test 
Box.test(std_resid, type = "Ljung-Box")                                                 # Ljung-Box test
# accept H0 --> there is not residual autocorrelation 

### gjrGARCH(1,1) ###
spec       = ugarchspec(mean.model = list(armaOrder = c(0,0)),                          # gjrGARCH(1,1) specifications
                        variance.model = list(model = 'gjrGARCH', garchOrder = c(1,1)),
                        distribution.model = 'norm')
gjrgarch.fit = ugarchfit(data = as.array(X), spec = spec)                               # gjrGARCH(1,1) fitting
gjrgarch.fit

# Diagnostic 
resid <- residuals(gjrgarch.fit)                                                        # Extract the residuals
std_resid <-  resid / sigma(gjrgarch.fit)                                               # Standardized 
mean(std_resid)                                                                         # Mean
var(std_resid)                                                                          # Variance 
skewness(std_resid)                                                                     # Skewness
kurtosis(std_resid)                                                                     # Kurtosis 
jarque.bera.test(std_resid) 
# reject H0 --> standardized residuals are not normally distributed 

# QQ-plot (to highlight the presence of fat tails)
qqnorm(std_resid)                                                                              
qqline(std_resid) 

# Distribution plot vs standard normal (same procedure as above for the distribution plot)
df <- data.frame(x = std_resid) 
ggplot(df, aes(x = x)) +                                                               
  geom_density(fill = "blue", alpha = 0.2) +                                            
  stat_function(fun = dnorm, args = list(mean = 0, 
                                         sd = 1), color = "red") +                 
  labs(title = "gjrGARCH(1,1) std resid vs std normal", x = "", y = "Density", 
       color = "Distribution") +                                                       
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Empirical", "Theoretical")) +
  theme(plot.title = element_text(family = font_name, size = font_size, face = "bold", hjust = 0.5),
        axis.title = element_text(family = font_name, size = font_size),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey", size = 0.20),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.20),
        axis.line = element_line(color = "lightgrey"))                        # Set colors


# Ljung-Box test 
Box.test(std_resid, type = "Ljung-Box")                                                 # Ljung-Box test
# accept H0 --> there is not residual autocorrelation 

#AIC BIC comparions
infocriteria(sgarch.fit)

infocriteria(sgarchM.fit)

infocriteria(gjrgarch.fit)

infocriteria(egarch.fit)
#We can also compare different lags of BIC and AIC specifications.
