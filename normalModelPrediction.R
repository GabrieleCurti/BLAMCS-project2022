library('rjags')
library('fastDummies')

rm(list=ls())

setwd("~/GitHub/BLAMCS-project2022")
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
#ford2$fuelType_Hybrid <- NULL
#ford2$`transmission_Semi-Auto` <- NULL
#ford2$`model_ Grand Tourneo Connect` <- NULL #Who bought this?

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
x <- train[,-18]
x <- train[,-21]
x <- train[,-26]
x <- train[,-29]
yp <- test$price
xp <- test[,-2]
xp <- train[,-18]
xp <- train[,-21]
xp <- train[,-26]
xp <- train[,-29]

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
      muz[i] <- zbeta0 + inprod(zx[i,], zbeta)
      mu[i] <- muz[i] * ysd + ym # Original Scale
    } 
    
    # Prior
    zbeta0 ~ dnorm(0, 1/4) # Since data are normalized, they are in [-1, 1]
    
    for(j in 1:p) {
    	zbeta[j] ~ dnorm(0, 1/4)
    }
    
    zsigma ~ dunif( 1.0E-5 , 1.0E+1 )
    prec <- 1 / (zsigma*zsigma)
    
    #Prediction
    for(t in 1:Ntest){
      zyp[t] ~ dnorm(zbeta0 + inprod(zxp[t,], zbeta), prec)
      yp[t] <- zyp[t] * ysd + ym # Original scale
    }
    
    # Transform to original scale:
    beta[1:p] <- ( zbeta[1:p] / xsd[1:p] )*ysd
    beta0 <- zbeta0*ysd  + ym - sum( zbeta[1:p] * xm[1:p] / xsd[1:p] )*ysd
    sigma <- zsigma*ysd
    
    # calculate R^2
    varFit <- (sd(muz))^2
    varRes <- zsigma^2 # get the variance of residuals
    R2 <- varFit / (varFit + varRes)
  }
  "
  , file = "models/predictionNormalJags.bug")

params <- c("beta0", "beta", "sigma", "yp", "R2", "mu")

jagsdata = list(N = N, y = y, x = x, p = p, xp = xp, Ntest = Ntest)

# Compile
fit <- jags.model(file = "models/predictionNormalJags.bug",
                  data = jagsdata, n.adapt = 500, n.chains = 1)

# Burn-in
update(fit, n.iter = 1000)

# Sample
results <- coda.samples(fit, variable.names = params,
                        n.iter = 5000, thin = 10)

#save(results, file='chains/predictionNormal3Chains.dat')

load("chains/allCovariates/betasAndStuff.dat")
allMatrix <- as.matrix(betasMCMC)
print(mean(allMatrix[,"R2"]))

load("chains/basSelection/betasAndStuff.dat")
allMatrix <- as.matrix(betasMCMC)
print(mean(allMatrix[,"R2"]))

load("chains/spikeNSlab5/betasAndStuff.dat")
allMatrix <- as.matrix(betasMCMC)
print(mean(allMatrix[,"R2"]))

load("chains/spikeNSlab6/betasAndStuff.dat")
allMatrix <- as.matrix(betasMCMC)
print(mean(allMatrix[,"R2"]))

load("chains/allCovariates/predictionOnTest.dat")
allMatrix <- as.matrix(predictionsTestMCMC)
yPred <- colMeans(allMatrix)
print(sqrt(mean((yPred-yp)^2)))


resultMatrix <- as.matrix(results)
beta <- resultMatrix[,grep("^beta$|^beta\\[",colnames(resultMatrix))]
mu <- resultMatrix[,grep("^mu$|^mu\\[",colnames(resultMatrix))]
ypred <- resultMatrix[,grep("^yp$|^yp\\[",colnames(resultMatrix))]
r2 <- resultMatrix[,"R2"]
sigma <- resultMatrix[,"sigma"]
print(mean(sigma))

ypredMean <- colMeans(as.matrix(ypred))

plot(ypredMean, yp)
rmse <- sqrt(mean((ypredMean-yp)^2))

muMean <- colMeans(as.matrix(mu))
plot(muMean, y)
rmseTrain <- sqrt(mean((muMean-y)^2))

print((rmse / mean(yp))*100)
print(rmseTrain)

# Compute R^2 for credible parameters:
YcorX = cor( y , x ) # correlation of y with each x predictor
zbeta = (mean(y)-beta) / sd(y)
Rsq = zbeta %*% matrix( YcorX , ncol=1 )
print(mean(Rsq))
print(mean(r2))
