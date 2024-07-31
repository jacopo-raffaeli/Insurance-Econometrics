install.packages("reshape2")
install.packages("gplots")
install.packages('Rcpp') 
library(plm)
require(ggplot2)
require(gplots)
library(reshape2)
library(Rcpp)

### Line of code which checks if the package is already installed
if(!require(plm)) install.packages("plm")
library("plm")

# Import dataset
rm(list = ls()) #Clean workspace
data("Grunfeld", package = "plm")

View(Grunfeld)
head(Grunfeld)

# Plot for each firm
p <- ggplot(data = Grunfeld)
p <- p + geom_line(aes(y = inv, x = year, col = factor(firm)))
p <- p + facet_wrap(~ firm, scales = "free")
p <- p + theme(legend.position="none")
print(p)

# Heterogeneity

?plotmeans
plotmeans(inv ~ factor(firm),
          main = "Heterogeneity across firms",
          data = Grunfeld)

plotmeans(inv ~ year,
          main = "Heterogeneity across years",
          data = Grunfeld)


##################################
##### Answer to question 1 #######
##################################

pooled_ols <- plm(inv ~ value + capital,
                  data = Grunfeld, model = "pooling")
summary(pooled_ols)


##################################
##### Answer to question 2 #######
##################################

# Remove the intercept (???1);
# Use function factor() for firm. The function factor() creates, in our case, a
# dummy variable for each firm.

LSDV <-lm(inv ~ value + capital + factor(firm) -1, data=Grunfeld)
summary(LSDV)

# The Least Squares Dummy Variable (LSDV) model provides a good way to understand 
# fixed effects. In this case we added an intercept for each firm.
# Each dummy is absorbing the particular effects to each firm.


##################################
##### Answer to question 3 #######
##################################

# Use function plm() with model = within.
library(Rcpp)
fixed_eff <- plm(inv ~ value + capital, data = Grunfeld, 
                    index = c("firm", "year"), 
                    effect = "individual", model = "within")

summary(fixed_eff)

fixef(fixed_eff)

##################################
##### Answer to question 4 #######
##################################

?pFtest
pFtest(fixed_eff, pooled_ols)

# H0: OLS better than fixed effects
# Considering a significance level of 5% and the p-value of the test, we can reject
# the null hypothesis. Hence, the Fixed Effects model is a better choice with respect
# to the Pooled OLS.


##################################
##### Answer to question 5 #######
##################################

random_eff <- plm(inv ~ value + capital, data = Grunfeld, 
                    index = c("firm", "year"), 
                    effect = "individual", model = "random")

summary(random_eff)

##################################
##### Answer to question 7 #######
##################################

?phtest
# Checks whether the individual error terms are 
# correlated with the regressors.
# H0: there is no correlation (RE)
# H1: there is correlation (FE)

phtest(fixed_eff, random_eff)

# Considering a significance level of 5% and the p-value of the test, we cannot reject
# the null hypothesis. Hence, the Random Effects model is a better choice with
# respect to the Fixed Effects model.


##################################
##########    Bonus    ###########
##################################

# 1 - Pooled model can be enhanced by using a factor
# on year

pooled_ols_time_lm <- lm(inv ~ value + capital + factor(year), 
                         data = Grunfeld)
summary(pooled_ols_time_lm)

# The same in plm 

pooled_ols_time_plm <- plm(inv ~ capital + factor(year), data = Grunfeld, 
                          index = c("firm", "year"), effect = "individual",
                          model = "pooling")
summary(pooled_ols_time_plm)


# Plot estimated models
new.dt <- data.frame(Grunfeld)[, c("firm", "year", "inv")]
colnames(new.dt)[3] <- "Y.Historical"
firm_names <- paste0("Firm #", sort(unique(new.dt$firm)))
new.dt$firm <- paste0("Firm #", new.dt$firm)
new.dt <- data.frame(new.dt,
                     Y.Pooled_OLS = as.numeric(pooled_ols$model[[1]] - pooled_ols$residuals),
                     Y.Fixed_effects = as.numeric(fixed_eff$model[[1]] - fixed_eff$residuals),
                     Y.Random_effects = as.numeric(random_eff$model[[1]] - random_eff$residuals))
new.dt$firm <- factor(new.dt$firm, levels = firm_names)
new.dt <- melt(new.dt, id.vars = c("firm", "year"))
p <- ggplot(data = new.dt,
            aes(x = year, y = value, group = interaction(variable, firm)))
p <- p + geom_line(aes(color = variable, linetype = variable))
p <- p + facet_wrap(~ firm, scales = "free")
p <- p + theme(legend.position="bottom")
p <- p + ggtitle("")
print(p)
