library('rjags')
library('fastDummies')

rm(list=ls())

#setwd("~/BLAMCS-project2022")
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

#Dropping one of each dummy variable since they are linearly reconstructable using a XOR
ford2$fuelType_Hybrid <- NULL
ford2$`transmission_Semi-Auto` <- NULL
ford2$`model_ Grand Tourneo Connect` <- NULL #Who bought this?

summary(ford2)

#Transforming ordinal attributes using ordinal variable enconding
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

ford2$year <- encode_ordinal(ford2$year)

#ford2 <- read.table("fordNoOutliers.dat", header=T)

# Split the dataset 70% for training, 30% for testing
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(ford2), replace=TRUE, prob=c(0.7,0.3))
train  <- ford2[sample, ]
test   <- ford2[!sample, ]

#save(train, file="data/ford_train_NoOutlier.dat")
#save(test, file="data/ford_test_NoOutlier.dat")

# Trying a simple linear regression
Y <- train$price
X <- train[,-2]

FordLM = lm(price~., data=train)
summary(FordLM)

testX <- test[,-2]
testY <- test$price

predictedTrain <- predict.lm(object=FordLM, newdata = X)
rmseTrain <- sqrt(mean((predictedTrain-Y)^2))
print(rmseTrain)

predictedTest <- predict.lm(object=FordLM, newdata=testX)
rmseTest <- sqrt(mean((predictedTest-testY)^2))
print(rmseTest)
print((rmseTest / mean(ford2$price))*100)
