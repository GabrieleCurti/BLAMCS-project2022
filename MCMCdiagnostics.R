# # Once the JAGS output is obtained it is useful to check the following:
# 1. bias: are the draws representative of the whole posterior distribution?
# 2. precision: do we have enough draws to get sufficiently precise summary statistics?

#--------------------------------SET UP----------------------------------------#
library(coda)

# Load chains
load('chains/spikeNSlab5NoOut/betasAndStuff.dat')
mcmc_chains <- betasMCMC
print(mcmc_chains)
summary(mcmc_chains)

mcmc_Matrix <- as.matrix(mcmc_chains)
print(mcmc_Matrix)

mcmc_array <- as.array(mcmc_chains)
dim(mcmc_array)

dimnames(mcmc_array)
#------------------------------------------------------------------------------#



################################### 
########### CONVERGENCE ########### 
################################### 

# A Markov chain is a sequence of numbers where each one depends on the one 
# before. The very first value is a random draw from the prior distribution, 
# unless we provide specific initial values. Then the chain moves over the 
# posterior distribution, eventually converging on the target distribution. 

# we check the convergence using the Gelman-Rubin diagnostic.Gelman-Rubin 
# measures whether there is a significant difference between the variance within 
# several chains and the variance between several chains by a value that is called
# “scale reduction factors”. 

gelman.diag(mcmc_chains)
gelman.plot(mcmc_chains)


# gelman.diag() calculates the `potential scale reduction factor' for each 
# variable in the mcmc.list's chains, together with upper and lower confidence 
# limits. Approximate convergence is diagnosed when the upper limit is close to 1.

# For multivariate chains, a multivariate value is calculated that bounds above 
# the potential scale reduction factor for any linear combination of the 
# (possibly transformed) variables.

# NB: "psrf" stand for 'potential scale reduction factor'


# gelman.plot shows the evolution of Gelman and Rubin's shrink factor as the 
# number of iterations increases.

library(bayesplot)

color_scheme_set("mix-brightblue-gray")
mcmc_trace(mcmc_chains, window = c(400,500)) +
  xlab("Post-warmup iteration")

# From the traces of the parameter vectors we can conclude that beta[18] and 
# beta[19] show a significant level of autocorrelation between the different 
# subsequent draws (there seems to be few independent observations in our 
# sample), as consequence of that the parameter space has been explored 
# only few times. A possible solution might be increasing our sample size.

# The plots show instead no apparent anomalies and can be regarded as
# 'healthy chains'

# We can use the Autocorrelation function (ACF) plots to further inspect the 
# trace plots in order to measure more precisely the serial correlation

autocorr(mcmc_chains, lags = c(0, 1, 5, 10, 50), relative=TRUE)
autocorr.plot(mcmc_chains, 100, auto.layout = TRUE, color = "blue")

# From the autocorrelation plots of beta[18] and beta[19] we can see a large 
# positive correlation between samples that dies out quite slowly to become 
# negative around 600 samples.