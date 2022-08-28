
#--------------------------------SET UP----------------------------------------#
setwd("/Users/monti/Desktop/BLAMCS-project2022")
library("bayesplot")
library("ggplot2")
library("rstanarm")
#------------------------------------------------------------------------------#

# Load chains
load('chains/modelSelection.dat')
print(results)
summary(results)

resultMatrix <- as.matrix(results)
print(resultMatrix)

posterior <- as.array(results)
dim(posterior)

dimnames(posterior)


#---------------------POSTERIOR UNCERTANTY INTERVALS---------------------------#

# Plots of credible intervals using quantiles

color_scheme_set("red")
mcmc_intervals(posterior, pars = c("beta[1]", "beta[4]", "beta[5]", "beta[6]", "sigma"))

# The points in the plots represent the MEDIAN, the thick segment represent the
# 50% while the thin segment represents the 90%


# Credible intervals as shaded area below the posterior density:

color_scheme_set("green")
mcmc_areas(
  posterior, 
  pars = c("beta[1]", "beta[4]", "beta[5]", "beta[6]"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)
#------------------------------------------------------------------------------#


#----------------UNIVARIATE MARGINAL POSTERIOR DISTRIBUTIONS-------------------#

# The mcmc_hist function plots marginal posterior distributions 
# (combining all chains), it is possible to plot more than two covariates

###########################
#       HISTOGRAMS
###########################

color_scheme_set("green")
mcmc_hist(posterior, pars = c("beta[1]", "beta[3]"))


# It is possible to plot log(xxx) rather than xxx as-is using the transformation
# parameter:

mcmc_hist(posterior, pars = c("beta[1]", "beta[3]", "sigma"),
          transformations = list("beta[1]" = "log", "beta[3]" = "log", "sigma" = "log"))

# In case of multiple Markov chains -> To view separate histograms of each of 
# the four Markov chains we can use mcmc_hist_by_chain, which plots each chain 
# in a separate facet in the plot.

# NB: The function REQUIRES multiple chains. It WILL NOT WORK with only one 
# Markov chain

color_scheme_set("brightblue")
mcmc_hist_by_chain(posterior, pars = c("beta[1]", "beta[3]"))


###########################
#       DENSITIES
###########################

color_scheme_set("purple")
mcmc_dens(posterior, pars = c("beta[1]", "beta[4]", "sigma"))



######################################################
#       DENSITIES OVERLAY w/ MULTIPLE CHAINS
######################################################

# Like mcmc_hist_by_chain, the mcmc_dens_overlay function separates the Markov 
# chains. But instead of plotting each chain individually, the density estimates
# are overlaid.

mcmc_dens_overlay(posterior, pars = c("wt", "sigma"))



###########################
#         VIOLIN
###########################

# The mcmc_violin function plots the density estimates of each chain as violins
# and draws horizontal line segments at user-specified quantiles.

# NB: works with multiple chains ONLY

color_scheme_set("teal")
mcmc_violin(posterior, pars = c("beta[1]", "sigma"), probs = c(0.1, 0.5, 0.9))

#------------------------------------------------------------------------------#




#-----------------------------BIVARIATE PLOTS----------------------------------#

###########################
#         SCATTER
###########################

# The mcmc_scatter function creates a simple scatterplot of two parameters.

color_scheme_set("green")
mcmc_scatter(posterior, pars = c("sigma", "alpha"), size = 1.5, alpha = 0.5)


###########################
#        HEXAGON
###########################

# The mcmc_hex function creates a similar plot but using hexagonal binning,
# which can be useful to avoid overplotting.


# requires hexbin package
if (requireNamespace("hexbin", quietly = TRUE)) {
  mcmc_hex(posterior, pars = c("sigma", "alpha"))
}



####################################
#        PAIRS CORRELATION
####################################

# In addition to mcmc_scatter and mcmc_hex, bayesplot now provides an mcmc_pairs
# function for creating pairs plots with more than two parameters.

# NB: works better with multiple chains

mcmc_pairs(posterior, pars = c("beta[1]", "beta[2]", "beta[3]"),
           off_diag_args = list(size = 1.5))


# DESCRIPTION:
# The univariate marginal posteriors are shown along the diagonal as histograms,
# but this can be changed to densities by setting diag_fun="dens". Bivariate 
# plots are displayed above and below the diagonal as scatterplots, but it is 
# also possible to use hex plots by setting off_diag_fun="hex". By default, 
# mcmc_pairs shows some of the Markov chains (half, if an even number of chains)
# above the diagonal and the others below. 

#------------------------------------------------------------------------------#