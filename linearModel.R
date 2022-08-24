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

# Split the dataset 70% for training, 30% for testing
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(ford2), replace=TRUE, prob=c(0.7,0.3))
train  <- ford2[sample, ]
test   <- ford2[!sample, ]

# Normalize function
normalize <- function(x) {
  col <- 5
  if (is.null(ncol(x))) {
    x <- (x-mean(x)) / sd(x)
  } else {
    for (i in 1:col) {
      print(mean(x[,i]))
      x[,i] <- (x[,i]-mean(x[,i])) / sd(x[,i])
    }
  }
  x
}

# Trying a simple linear regression
Y <- train$price
yMean = mean(Y)
ySd = sd(Y)
Y <- normalize(Y)

X <- train[,-2]
X <- normalize(X)
summary(X)

X <- as.matrix(X)
Y <- as.vector(Y)
# Get dimensions
N <- dim(X)[1]
p <- dim(X)[2]

#Jags model
cat(
  "
  model {
    # Likelihood 
    for (i in 1:N) {
      Y[i] ~ dnorm(mu[i], prec)
      mu[i] <- alpha + inprod(X[i,], beta)
    } 
    
    # Prior
    alpha ~ dnorm(0, 0.001)
    
    for(j in 1:p) {
    	beta[j] ~ dnorm(0, 0.001)
    }

    sigma ~ dgamma(0.01, 0.01)
    prec <- 1 / (sigma*sigma)
  }
  "
  , file = "models/simpleJags.bug")

params <- c("alpha", "beta", "sigma")

jagsdata = list(N = N, Y = Y, X = X, p = p)

# Compile
fit <- jags.model(file = "models/simpleJags.bug",
                  data = jagsdata, n.adapt = 1000)

# Burn-in - done with the n.adapt phase
# update(fit, n.iter = 1000)

# Sample
results <- coda.samples(fit, variable.names = params,
                        n.iter = 5000, thin = 10)

save(results, file='linearModelChain.dat')

summary(results)
plot(results[,3])
resultMatrix <- as.matrix(results)


post_means <- colMeans(as.matrix(results))
print(post_means)
q = post_means["alpha"]
m = as.matrix(post_means)
m = post_means[-1]
m = m[-27]

prediction = q + X %*% m
plot(prediction, Y)
rmse <- sqrt(mean((prediction-Y)^2))

space = 1:N

plot(Y)
lines(space, Y)
lines(space, prediction)
