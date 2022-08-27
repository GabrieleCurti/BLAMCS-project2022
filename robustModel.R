library('rjags')
library('fastDummies')

rm(list=ls())

setwd("~/Documents/GitHub/BLAMCS-project2022")
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

# Trying a simple linear regression with all features
y <- train$price
x <- train[,-2]
yp <- test$price
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
      y[i] ~ dt( mu[i] , prec , nu )
      mu[i] <- zbeta0 + inprod(x[i,], zbeta)
    } 
    
    # Prior
    zbeta0 ~ dnorm(0, 1/4) # Since data are normalized, they are in [-1, 1]
    
    for(j in 1:p) {
    	zbeta[j] ~ dnorm(0, 1/4)
    }
    
    zsigma ~ dunif( 1.0E-5 , 1.0E+1 )
    prec <- 1 / (zsigma*zsigma)
    
    nu ~ dexp (1/30.0)
    
    #Prediction
    for(t in 1:Ntest){
      yp[t] ~ dt(zbeta0 + inprod(xp[t,], zbeta), prec, nu)
    }
    
    # Transform to original scale:
    beta[1:p] <- ( zbeta[1:p] / xsd[1:p] )*ysd
    beta0 <- zbeta0*ysd  + ym - sum( zbeta[1:p] * xm[1:p] / xsd[1:p] )*ysd
    sigma <- zsigma*ysd
  }
  "
  , file = "models/predictionStudentJags.bug")

params <- c("beta0", "beta", "sigma", "yp")

jagsdata = list(N = N, y = y, x = x, p = p, xp = xp, Ntest = Ntest)

# Compile
fit <- jags.model(file = "models/predictionStudentJags.bug",
                  data = jagsdata, n.adapt = 500)

# Burn-in
update(fit, n.iter = 1000)

# Sample
results <- coda.samples(fit, variable.names = params,
                        n.iter = 10000, thin = 2)

save(results, file='chains/predictionStudentChain.dat')

post_means <- colMeans(as.matrix(results))
print(post_means)

m = as.matrix(post_means)
ypred = m[29:2315]

plot(ypred, yp)
rmse <- sqrt(mean((ypred-yp)^2))
print((rmse / mean(yp))*100)
