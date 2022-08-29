# Import library
library("BAS")
library('fastDummies')

rm(list=ls())

setwd("C:/Users/Pc/Documents/GitHub/BLAMCS-project2022")
ford <- read.table("ford.txt", header=T)

summary(ford)

modelCategories <- unique(ford$model)
transmissionCategories <- unique(ford$transmission)
fueltCategories <- unique(ford$fuelType)
yearCat <- unique(ford$year)

# Transform categorical attributes using dummy variable encoding
ford2 <- ford

ford2 <- dummy_cols(ford2, select_columns = "transmission")
ford2 <- dummy_cols(ford2, select_columns = "model")
ford2 <- dummy_cols(ford2, select_columns = "fuelType")

ford2$fuelType <- NULL
ford2$model <- NULL
ford2$transmission <- NULL

# Use `bas.lm` to run regression model
price.BIC = bas.lm(price ~ ., data = ford2,
                  prior = "BIC", modelprior = uniform())

round(summary(price.BIC), 3)

# Find the index of the model with the largest logmarg
best = which.max(price.BIC$logmarg)
# Retreat the index of variables in the best model, 0 is the intercept index
bestmodel = price.BIC$which[[best]]+1

print(bestmodel)

# 0 vector with length equal to the number of variables in the full model
bestgamma = rep(0, price.BIC$n.vars)
# Change the indicator to 1 where variables are used
bestgamma[bestmodel] = 1

print(bestgamma)


# Fit the best BIC model. Impose the variables to use via bestgamma
price.bestBIC = bas.lm(price ~ ., data = ford2, prior = "BIC",
                     modelprior=uniform(), n.models=1, bestmodel=bestgamma)

# Retreat coefficients information
price.coef = coef(price.bestBIC)

# Retreat bounds of credible intervals
out = confint(price.coef)[, 1:2]

# Combine results and construct summary table
coef.BIC = cbind(price.coef$postmean, price.coef$postsd, out)
names = c("post mean", "post sd", colnames(out))
colnames(coef.BIC) = names

round(coef.BIC[bestmodel,], 3)

# Plot best regressors
par(mfrow=c(1,2))
plot(price.coef, subset = (bestmodel)[-1], ask = F)

