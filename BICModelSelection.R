# Import library
library("BAS")
library('fastDummies')

rm(list=ls())

setwd("C:/Users/Pc/Documents/GitHub/BLAMCS-project2022")
load("data/ford_train_NoOutlier.dat")

summary(train)

modelCategories <- unique(train$model)
transmissionCategories <- unique(train$transmission)
fueltCategories <- unique(train$fuelType)
yearCat <- unique(train$year)


# Use `bas.lm` to run regression model
price.BIC = bas.lm(price ~ ., data = train,
                  prior = "BIC", modelprior = uniform())

# Find the index of the model with the largest logmarg
best = which.max(price.BIC$logmarg)
# Retreat the index of variables in the best model, 0 is the intercept index
bestmodel = price.BIC$which[[best]]

print(bestmodel)

# 0 vector with length equal to the number of variables in the full model
bestgamma = rep(0, price.BIC$n.vars)
# Change the indicator to 1 where variables are used
bestgamma[bestmodel + 1] = 1

print(bestgamma)

ford_BASmodel_NoOutlier <- train[bestmodel]

save(ford_BASmodel_NoOutlier, file = "data/ford_BASmodel_NoOutlier.dat")