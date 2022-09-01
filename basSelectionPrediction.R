library('rjags')

rm(list=ls())

# Load train and test
load("data/ford_BASmodel_NoOutlier.dat")
load("data/ford_BASmodel_NoOutlier_test.dat")

# Trying a simple linear regression with all features
y <- train$price
x <- train[,-2] # Dropping target
yp <- test$price
xp <- test[,-2] # Dropping target

x <- as.matrix(x)
y <- as.vector(y)
# Get dimensions
N <- dim(x)[1]
p <- dim(x)[2]
Ntest <- dim(test)[1]

#JAGS model already defined in allCovariatesPrediction.r

# Save betas and hyper parameters
params <- c("alpha", "beta", "sigma", "R2", "yp", "mu")

jagsdata = list(N = N, y = y, x = x, p = p, xp = xp, Ntest = Ntest)

# Compile
fit <- jags.model(file = "models/predictionNormalJags.bug",
                  data = jagsdata, n.adapt = 500, n.chains = 3)

# Burn-in
update(fit, n.iter = 1000)

# Sample
results <- coda.samples(fit, variable.names = params,
                        n.iter = 5000, thin = 10)

betasMCMC <- results[,grep("alpha|sigma|R2|^beta",colnames(results[[1]]))]
predictionsTestMCMC <- results[,grep("^yp",colnames(results[[1]]))]
predictionsTrainMCMC <- results[,grep("^mu",colnames(results[[1]]))]

save(betasMCMC, file='chains/basSelectionNoOut/betasAndStuff.dat')
save(predictionsTestMCMC, file='chains/basSelectionNoOut/predictionOnTest.dat')
save(predictionsTrainMCMC, file='chains/basSelectionNoOut/predictionOnTrain.dat')
