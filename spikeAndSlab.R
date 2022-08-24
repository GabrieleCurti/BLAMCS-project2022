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
    	beta_temp[j] ~ dnorm(0, 0.001)
    	g[j] ~ dbern(theta[j])
    	theta[j] ~ dunif(0,1)
    	beta[j] <- g[j] * beta_temp[j]	
    }

    sigma ~ dgamma(0.01, 0.01)
    prec <- 1 / (sigma*sigma)
  }
  "
  , file = "models/simpleJags.bug")

params <- c("alpha", "beta", "sigma", "g")

jagsdata = list(N = N, Y = Y, X = X, p = p)

# A list of initial value for the MCMC algorithm 
inits = function() {
  list(alpha = 0.0, beta_temp = rep(0,p), g = rep(0,p), theta = rep(0.5, p),
       .RNG.seed = 321, .RNG.name = 'base::Wichmann-Hill') 
}

# Compile
fit <- jags.model(file = "models/spikeAndSlab.bug",
                  data = jagsdata, n.adapt = 1000, inits = inits, n.chains = 1)


# Burn-in - done with the n.adapt phase
# update(fit, n.iter = 1000)

# Sample
results <- coda.samples(fit, variable.names = params,
                        n.iter = 5000, thin = 10)

save(results, file='chains/spikeAndSlab.dat')

# Load chains
load('chains/spikeAndSlab.dat')

# Look at the structure of the output is an mcmc object of the library coda
str(results)

K <- 1
plot(results[,(4*K+1):(4*(K+1))])
summary(results)

resultMatrix <- as.matrix(results)

post_g <- resultMatrix[,28:53]
apply(post_g, 2, "mean")
post_mean_g <- apply(post_g, 2, "mean") 

#Plot of g
library(ggplot2)
df <- data.frame(value = post_mean_g, var = colnames(X))
p1 <- ggplot(data = df, aes(y = value, x = var, fill = var)) + 
  geom_bar(stat="identity") + 
  geom_hline(mapping = aes(yintercept = .5), col = 2, lwd = 1.1) +
  coord_flip() + theme_minimal() + theme(legend.position="none") + 
  ylab("Posterior Inclusion Probabilities") + xlab("")
p1


######################################


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
