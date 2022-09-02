# The following file contains the guidelines to conduct the analysis of the 
# posterior distributions obtained from the JAGS model regression. 
# NB: an mcmc object is necessary in order for the plot function to work properly


#--------------------------------SET UP----------------------------------------#
# Set working directory:
#setwd("/Users/monti/Desktop/BLAMCS-project2022")

######## DA INSTALLARE: ######## 
install.packages("bayesplot")
install.packages("ggplot2")
install.packages("rstanarm")
################################ 

# Load libraries
library("bayesplot")
library("ggplot2")
library("rstanarm")
#------------------------------------------------------------------------------#

# Load chains
load('chains/spikeNSlab5NoOut/betasAndStuff.dat')
results <- betasMCMC
print(results)
summary(results)

mcmc_Matrix <- as.matrix(results)
print(mcmc_Matrix)

mcmc_array <- as.array(results)
dim(mcmc_array)

dimnames(mcmc_array)

betas_to_plot = c("beta[1]", "beta[4]", "beta[5]", "beta[6]")


#---------------------POSTERIOR UNCERTANTY INTERVALS---------------------------#

# Plots of credible intervals using quantiles

color_scheme_set("red")
mcmc_intervals(mcmc_array)

# The points in the plots represent the MEDIAN, the thick segment represent the
# 50% while the thin segment represents the 90%


# Credible intervals as shaded area below the posterior density:

color_scheme_set("green")
mcmc_areas(
  mcmc_array, 
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
mcmc_hist(mcmc_array, pars = betas_to_plot)


# In case of multiple Markov chains -> To view separate histograms of each of 
# the four Markov chains we can use mcmc_hist_by_chain, which plots each chain 
# in a separate facet in the plot.

# NB: The function REQUIRES multiple chains. It WILL NOT WORK with only one 
# Markov chain

color_scheme_set("brightblue")
mcmc_hist_by_chain(mcmc_array, pars = c("beta[1]", "beta[3]"))


###########################
#       DENSITIES
###########################

color_scheme_set("purple")
mcmc_dens(mcmc_array, pars = betas_to_plot)



######################################################
#       DENSITIES OVERLAY w/ MULTIPLE CHAINS
######################################################

# Like mcmc_hist_by_chain, the mcmc_dens_overlay function separates the Markov 
# chains. But instead of plotting each chain individually, the density estimates
# are overlaid.

mcmc_dens_overlay(mcmc_array)



###########################
#         VIOLIN
###########################

# The mcmc_violin function plots the density estimates of each chain as violins
# and draws horizontal line segments at user-specified quantiles.

# NB: works with multiple chains ONLY

color_scheme_set("orange")
mcmc_violin(mcmc_array, pars = betas_to_plot, probs = c(0.1, 0.5, 0.9))

#------------------------------------------------------------------------------#




#-----------------------------BIVARIATE PLOTS----------------------------------#

###########################
#         SCATTER
###########################

# The mcmc_scatter function creates a simple scatterplot of exactly TWO parameters.
# Depending on how tightly the points cluster together, you may be able to 
# discern a clear trend in the data.

# Is possible to see how strongly the covariates are correlated: the closer the 
# data come to forming a straight line when plotted, the higher the correlation 
# between the two variables.
# 
# If the data points make a straight line going from near the origin out to high 
# y-values, the variables are said to have a positive correlation. If the data 
# points start at high y-values on the y-axis and progress down to low values, the 
# variables have a negative correlation.

color_scheme_set("green")
mcmc_scatter(mcmc_array, pars = c("sigma", "alpha"), size = 1.5, alpha = 0.5)


###########################
#        HEXAGON
###########################

# The mcmc_hex function creates a similar plot but using hexagonal binning,
# which can be useful to avoid overplotting.

# install.package("hexbin")
# library("hexbin")

# It requires hexbin package
if (requireNamespace("hexbin", quietly = TRUE)) {
  mcmc_hex(mcmc_array, pars = c("sigma", "alpha"))
}



####################################
#        PAIRS CORRELATION
####################################

# In addition to mcmc_scatter and mcmc_hex, bayesplot now provides an mcmc_pairs
# function for creating pairs plots with more than two parameters.

# NB: works better with multiple chains

mcmc_pairs(mcmc_array,
           off_diag_args = list(size = 1.5))


mcmc_pairs(mcmc_array,
           off_diag_args = list(size = 1.5),
           diag_fun = "dens",
           off_diag_fun = "hex")


# DESCRIPTION:
# The univariate marginal posteriors are shown along the diagonal as histograms,
# but this can be changed to densities by setting diag_fun="dens". Bivariate 
# plots are displayed above and below the diagonal as scatterplots, but it is 
# also possible to use hex plots by setting off_diag_fun="hex". By default, 
# mcmc_pairs shows some of the Markov chains (half, if an even number of chains)
# above the diagonal and the others below. 

#------------------------------------------------------------------------------#