load("data/ford_train.dat")


# Check pairwise correlation
install.packages("corrplot")
library("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
rquery.cormat(train, type = "full")

# plot prices vs. milage and year
ggplot(train, 
       aes(x = train$mileage, 
           y = train$price,
           color = train$year)) +
  geom_point() + 
  labs(title = "ford car prices by mileage and year")

######################

# It makes sense to inspect the relations between $price and variables like 
# [year, , transmission_Automatic, engine_Size,
# transmission_Semi-Auto, milage, mpg]


install.packages("BayesianTools")
library(BayesianTools)

keeps <- c("price","mileage", "mpg", "engineSize", "year")
data_for_correlation = train[keeps]


correlationPlot(data_for_correlation)

# plot analysis: Marginal densities (diagonal), pairwise densities (lower panes)
# and correlation coefficient (upper panels) for the fit with unbalanced x-values


# 1. price-mileage correlation #################################################

price_mileage <- cbind(test$price, test$mileage)

#install.packages("ggplot2")
library("ggplot2")

#qplot(train$price, train$mileage, geom=c("hex"))

frame_price_mileage = as.data.frame(price_mileage)
ggplot(frame_price_mileage, aes(x=V1, y=V2) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

# 2. price-year correlation ####################################################

price_year <- cbind(test$price, test$year)

frame_price_year = as.data.frame(price_year)
ggplot(frame_price_year, aes(x=V1, y=V2) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

# 3. price-mpg correlation #####################################################

price_mpg <- cbind(test$price, test$mpg)

frame_price_mpg = as.data.frame(price_mpg)
ggplot(frame_price_mpg, aes(x=V1, y=V2) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

# 4. price-engineSize correlation ##############################################

price_engineSize <- cbind(test$price, test$engineSize)

frame_price_engineSize = as.data.frame(price_engineSize)
ggplot(frame_price_engineSize, aes(x=V1, y=V2) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


# 5. mpg-mileage correlation ###################################################

mileage_mpg <- cbind(test$mileage, test$mpg)

frame_mileage_mpg = as.data.frame(mileage_mpg)
ggplot(frame_mileage_mpg, aes(x=V1, y=V2) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

################################################################################
################################################################################