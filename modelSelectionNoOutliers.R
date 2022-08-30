library('rjags')
library('fastDummies')

rm(list=ls())

setwd("C:/Users/Hp/Documents/GitHub/BLAMCS-project2022")
load("data/fordNoOutliers.dat")
ford2 <- fordNoOutliers

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

save(train, file='data/ford_train_NoOutlier.dat')
save(test, file='data/ford_test_NoOutlier.dat')


load("data/ford_test_NoOutlier.dat")
load("data/ford_train_NoOutlier.dat")

# Trying a simple linear regression
Y <- train$price
X <- train[,-2]

X <- as.matrix(X)
Y <- as.vector(Y)
# Get dimensions
N <- dim(X)[1]
p <- dim(X)[2]

#Jags model
cat(
  "
  data {
    # Standardize target
    ym <- mean(Y)
    ysd <- sd(Y)
    for ( i in 1:N ) {
      zy[i] <- ( Y[i] - ym ) / ysd
    }
    # Standardize covariates
    for ( j in 1:p ) {
      xm[j]  <- mean(X[,j])
      xsd[j] <- ifelse(sd(X[,j]) > 0, sd(X[,j]), 0.00001)
      
      for ( i in 1:N ) {
        zx[i,j] <- ( X[i,j] - xm[j] ) / xsd[j]
      }
    }
  }
  
  model {
    # Likelihood 
    for (i in 1:N) {
      zy[i] ~ dnorm(mu[i], prec)
      mu[i] <- alpha + inprod(zx[i,], beta)
    }
    
    # Prior
    alpha ~ dnorm(0, 0.001)
    
    #Normal prior
    for(j in 1:p) {
    	beta_temp[j] ~ dnorm(0, 0.001)
    	g[j] ~ dbern(theta[j])
    	theta[j] ~ dunif(0,1)
    	beta[j] <- g[j] * beta_temp[j]	
    }

    sigma ~ dgamma(0.01, 0.01)
    prec <- 1 / (sigma*sigma)
  }
  "
  , file = "models/modelSelectionNoOutliers.bug")

params <- c("alpha", "beta", "sigma", "g")

jagsdata = list(N = N, Y = Y, X = X, p = p)

# A list of initial value for the MCMC algorithm 
inits = function() {
  list(alpha = 0.0, beta_temp = rep(0,p), g = rep(0,p), theta = rep(0.5, p),
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

# Compile
fit <- jags.model(file = "models/modelSelectionNoOutliers.bug",
                  data = jagsdata, n.adapt = 1000, inits = inits, n.chains = 1)

# Burn-in
update(fit, n.iter = 1000)

# Sample
results <- coda.samples(fit, variable.names = params,
                        n.iter = 5000, thin = 10)

save(results, file='chains/modelSelectionNoOutliers.dat')

# Load chains
load('chains/modelSelectionNoOutliers.dat')

# Look at the structure of the output is an mcmc object of the library coda
str(results)

summary(results)

resultMatrix <- as.matrix(results)

post_g <- resultMatrix[,31:59]
apply(post_g, 2, "mean")
post_mean_g <- apply(post_g, 2, "mean") 

#Plot of g
library(ggplot2)
dev.off()
df <- data.frame(value = post_mean_g, var = colnames(X))
plot1 <- ggplot(data = df, aes(y = value, x = var, fill = var)) + 
  geom_bar(stat="identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() + theme_minimal() + theme(legend.position="none") + 
  ylab("Posterior Inclusion Probabilities") + xlab("")
plot1

cols <- colnames(X)

data5 <- as.matrix(ford2)
for(i in p:1) {
  if(post_mean_g[i] < 0.5) {
    data5 <- data5[,-i]
  }
}
save(data5, file='data/fordNoOutliers05.dat')

data6 <- as.matrix(ford2)
for(i in p:1) {
  if(post_mean_g[i] < 0.6) {
    data6 <- data6[,-i]
  }
}
save(data6, file='data/fordNoOutliers06.dat')
