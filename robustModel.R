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

# Trying a simple linear regression
y <- train$price
x <- train[,-2]
xp <- test[,-2]

x <- as.matrix(x)
y <- as.vector(y)
# Get dimensions
N <- dim(x)[1]
p <- dim(x)[2]
Ntest <- dim(test)[1]

#Jags model
cat(
  "
  # Normalize data:
  data {
    ym <- mean(y)
    ysd <- sd(y)
    for ( i in 1:N ) {
      zy[i] <- ( y[i] - ym ) / ysd
    }
    for ( j in 1:p ) {
      xm[j]  <- mean(x[,j])
      xsd[j] <-   sd(x[,j])
      for ( i in 1:N ) {
        zx[i,j] <- ( x[i,j] - xm[j] ) / xsd[j]
      }
    }
  }
  model {
    # Likelihood 
    for (i in 1:N) {
      y[i] ~ dnorm(mu[i], prec)
      mu[i] <- beta0 + inprod(X[i,], beta)
    } 
    
    # Prior
    beta0 ~ dnorm(0, 1/4) # Since data are normalized, they are [-1, 1]
    
    for(j in 1:p) {
    	beta[j] ~ dnorm(0, 1/4)
    }
    
    sigma ~ dunif( 1.0E-5 , 1.0E+1 )
    prec <- 1 / (sigma*sigma)
    
    #Prediction
    for(i in 1:Ntest){
      yP ~ dnorm(beta0 + inprod(Xp[i,], beta), prec)
    }
  }
  "
  , file = "models/predictionJags.bug")

params <- c("alpha", "beta", "sigma", "Xp", "yP")

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
