library('rjags')
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

#Dropping one of each dummy variable since they are linearly reconstructable using a XOR
#ford2$fuelType_Hybrid <- NULL
#ford2$`transmission_Semi-Auto` <- NULL
#ford2$`model_ Grand Tourneo Connect` <- NULL #Who bought this?

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
      xsd[j] <-   sd(X[,j])
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
  , file = "models/modelSelection.bug")

params <- c("alpha", "beta", "sigma", "g")

jagsdata = list(N = N, Y = Y, X = X, p = p)

# A list of initial value for the MCMC algorithm 
inits = function() {
  list(alpha = 0.0, beta_temp = rep(0,p), g = rep(0,p), theta = rep(0.5, p),
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

# Compile
fit <- jags.model(file = "models/modelSelection.bug",
                  data = jagsdata, n.adapt = 1000, inits = inits, n.chains = 1)


# Burn-in
update(fit, n.iter = 1000)

# Sample
results <- coda.samples(fit, variable.names = params,
                        n.iter = 5000, thin = 10)

save(results, file='chains/modelSelection.dat')

# Load chains
load('chains/modelSelection.dat')

# Look at the structure of the output is an mcmc object of the library coda
str(results)

K <- 1
summary(results)

resultMatrix <- as.matrix(results)

post_g <- resultMatrix[,31:59]
apply(post_g, 2, "mean")
post_mean_g <- apply(post_g, 2, "mean") 

#Plot of g
library(ggplot2)
df <- data.frame(value = post_mean_g, var = colnames(X))
plot1 <- ggplot(data = df, aes(y = value, x = var, fill = var)) + 
  geom_bar(stat="identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  geom_hline(mapping = aes(yintercept = .6), col = 4, lwd = 1.1) +
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
save(data5, file='data/ford05.dat')

data6 <- as.matrix(ford2)
for(i in p:1) {
  if(post_mean_g[i] < 0.6) {
    data6 <- data6[,-i]
  }
}
save(data6, file='data/ford06.dat')
