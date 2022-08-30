library('rjags')
library('fastDummies')

rm(list=ls())

load('data/ford05.dat')
load('data/ford06.dat')

ford2 <- data5
# Split the dataset 70% for training, 30% for testing
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(ford2), replace=TRUE, prob=c(0.7,0.3))
train  <- ford2[sample, ]
test   <- ford2[!sample, ]

# Trying a simple linear regression with all features
y <- train[,2]
x <- train[,-2] # Dropping target

yp <- test[,2]
xp <- test[,-2] # Dropping target

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
    # Standardize target
    ym <- mean(y)
    ysd <- sd(y)
    for ( i in 1:N ) {
      zy[i] <- ( y[i] - ym ) / ysd
    }
    # Standardize covariates
    for ( j in 1:p ) {
      xm[j]  <- mean(x[,j])
      xsd[j] <-   sd(x[,j])
      for ( i in 1:N ) {
        zx[i,j] <- ( x[i,j] - xm[j] ) / xsd[j]
      }
    }
    # Standardize test set!
    for ( j in 1:p ) {
      xpm[j]  <- mean(xp[,j])
      xpsd[j] <-   sd(xp[,j])
      for ( i in 1:Ntest ) {
        zxp[i,j] <- ( xp[i,j] - xpm[j] ) / xpsd[j]
      }
    }
  }
  model {
    # Likelihood 
    for (i in 1:N) {
      zy[i] ~ dnorm(muz[i], prec)
      muz[i] <- zalpha + inprod(zx[i,], zbeta)
      mu[i] <- muz[i] * ysd + ym # Original Scale
    } 
    
    # Prior
    zalpha ~ dnorm(0, 1/4) # Since data are normalized, they are in [-1, 1]
    
    for(j in 1:p) {
    	zbeta[j] ~ dnorm(0, 1/4)
    }
    
    zsigma ~ dunif( 1.0E-5 , 1.0E+1 )
    prec <- 1 / (zsigma*zsigma)
    
    #Prediction
    for(t in 1:Ntest){
      zyp[t] ~ dnorm(zalpha + inprod(zxp[t,], zbeta), prec)
      yp[t] <- zyp[t] * ysd + ym # Original scale
    }
    
    # Transform to original scale:
    beta[1:p] <- ( zbeta[1:p] / xsd[1:p] )*ysd
    alpha <- zalpha*ysd  + ym - sum( zbeta[1:p] * xm[1:p] / xsd[1:p] )*ysd
    sigma <- zsigma*ysd
    
    # calculate R^2
    varFit <- (sd(muz))^2
    varRes <- zsigma^2 # get the variance of residuals
    R2 <- varFit / (varFit + varRes)
  }
  "
  , file = "models/Y_monitoring_multipleChains.bug")

# Save betas and hyper parameters
params <- c("y", "alpha", "beta", "sigma")

jagsdata = list(N = N, y = y, x = x, p = p, xp = xp, Ntest = Ntest)

# Compile
fit <- jags.model(file = "models/Y_monitoring_multipleChains.bug",
                  data = jagsdata, n.adapt = 500, n.chains = 2)

# Burn-in
update(fit, n.iter = 1000)

# Sample
results <- coda.samples(fit, variable.names = params,
                        n.iter = 5000, thin = 10)

summary(results)


resultMatrix <- as.matrix(results)
print(resultMatrix)

posterior <- as.array(results)
dim(posterior)

dimnames(posterior)


betas_to_plot = c("beta[1]", "beta[4]", "beta[5]", "beta[6]")


color_scheme_set("red")
mcmc_intervals(posterior[1], pars = betas_to_plot)


