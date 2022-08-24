library('rjags')
library('fastDummies')
library('ggplot2')

rm(list=ls())

setwd("~/ProgettoFinaleBLAMCS")
ford <- read.table("ford.txt", header=T)

summary(ford)

modelCategories <- unique(ford$model)
transmissionCategories <- unique(ford$transmission)
fueltCategories <- unique(ford$fuelType)
yearCat <- unique(ford$year)
engineSize <- unique(ford$engineSize)
summary(ford$engineSize)

#Normalize data


#Split
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(ford), replace=TRUE, prob=c(0.7,0.3))
train  <- ford[sample, ]
test   <- ford[!sample, ]


N <- nrow(train)


#Using only mileage to price

X = train$mileage
X = (X - mean(X)) / sd(X)
Y = train$price
Y = (Y - mean(Y)) / sd(Y)

#Jags model
cat(
  "
  model {
    # Likelihood 
    for (i in 1:N) {
      Y[i] ~ dnorm(mu[i], prec)
      mu[i] <- alpha + beta * X[i]
    } 
    
    # Prior
    alpha ~ dnorm(0, 0.01)
    beta ~ dnorm(0, 0.01)
    sigma ~ dgamma(0.01, 0.01)
    prec <- 1 / sigma
  }
  "
  , file = "models/simpleJags.bug")

params <- c("alpha", "beta", "sigma")

jagsdata = list(N = N, Y = Y, X = X)

# Compile
fit <- jags.model(file = "models/simpleJags.bug",
                  data = jagsdata, n.adapt = 10000)

# Burn-in
update(fit, n.iter = 1000)

# Sample
results <- coda.samples(fit, variable.names = params,
                        n.iter = 2000, thin = 2)

summary(results)
plot(results)

post_means <- colMeans(as.matrix(results))
q = post_means["alpha"]
m = post_means["beta"]

prediction = q + m * X
plot(prediction, Y)
rmse <- sqrt(mean((prediction-Y)^2))

space = 1:N

curve = q + m * space

plot(Y)
lines(space, Y)
lines(space, prediction)
