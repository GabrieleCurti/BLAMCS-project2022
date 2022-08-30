# Import library
library("BAS")
library('fastDummies')

rm(list=ls())

setwd("C:/Users/Hp/Documents/GitHub/BLAMCS-project2022")
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

#Transforming ordinal attributes using ordinal variable enconding
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

ford2$year <- encode_ordinal(ford2$year)

boxplot(ford2[c("year", "price", "mileage", "tax", "mpg", "engineSize")])$out
#outliers for "year"
Q <- quantile(ford2$year, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(ford2$year)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

fordNoOutliers<- subset(ford2, ford2$year > (Q[1] - 1.5*iqr) & ford2$year < (Q[2]+1.5*iqr))


#outliers for "price"
Q <- quantile(ford2$price, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(ford2$price)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

fordNoOutliers<- subset(fordNoOutliers, fordNoOutliers$price > (Q[1] - 1.5*iqr) & fordNoOutliers$price < (Q[2]+1.5*iqr))


#outliers for "mileage"
Q <- quantile(ford2$mileage, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(ford2$mileage)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

fordNoOutliers<- subset(fordNoOutliers, fordNoOutliers$mileage > (Q[1] - 1.5*iqr) & fordNoOutliers$mileage < (Q[2]+1.5*iqr))


#outliers for "mpg"
Q <- quantile(ford2$mpg, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(ford2$mpg)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

fordNoOutliers<- subset(fordNoOutliers, fordNoOutliers$mpg > (Q[1] - 1.5*iqr) & fordNoOutliers$mpg < (Q[2]+1.5*iqr))


#outliers for "engineSize"
Q <- quantile(ford2$engineSize, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(ford2$engineSize)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

fordNoOutliers<- subset(fordNoOutliers, fordNoOutliers$engineSize > (Q[1] - 1.5*iqr) & fordNoOutliers$engineSize < (Q[2]+1.5*iqr))


#outliers for "tax"
mean = mean(ford2$tax)
sd = sd(ford2$tax)

Tmin = mean - (3*sd)
Tmax = mean + (3*sd)

fordNoOutliers<- subset(fordNoOutliers, fordNoOutliers$tax > Tmin & fordNoOutliers$tax < Tmax)


save(fordNoOutliers, file = "data/fordNoOutliers.dat")