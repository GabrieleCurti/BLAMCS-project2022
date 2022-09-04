# Bayesian Learning and Montecarlo Simulation Project

This repositories includes all the R scripts and data files we used. 

### Directory description
- *chains*: contains all the results from the execution of jags. They are divided by model and by parameter of interest.
	- *allCovariates*: model using all the features.
	- *allCovariatesNoOut*: model using all the features without outliers.
	- *basSelection*: model using features selected by BAS.
	- *basSelectionNoOut*: model using features selected by BAS without outliers.
	- *spikeNSlab5*: model using features selected by Spike and Slab with a 0.5 probability.
	- *spikeNSlab5NoOut*: model using features selected by Spike and Slab with a 0.5 probability without outliers.
	Inside each directory there are 3 files:
	- *betasAndStuff.dat*: keep samples from the parameters, R^2 and sigma.
	- *predictionOnTest.dat*: keep samples from the prediction computed on the test set.
	- *predictionOnTrain.dat*: keep samples from the prediction computed on the train set.
- *data*: contains the dataset transformation we used in the project. The most important file are:
	- *ford_test.dat*: test set including outliers.
	- *ford_train.dat*: train set including outliers.
	- *ford_test_noOutlier.dat*: test set excluding outliers.
	- *ford_train_noOutlier.dat*: train set excluding outliers.
- *images*: some new and outdated plots.
- *models*: JAGS model used for the various tasks:
	- *modelSelection.bug*: model used for running Spike And Slab model selection.
	- *predictionNormalJags.bug*: model used for running prediction tasks.

### R scripts description
- *allCovariatesPrediction.R*, *basSelectionPrediction.R*, *spikeAndSlabPrediction.R* files are used to compute prediction on the different cases. They use the same model but on different data, and save the chains they produce in the *chains* directory.
- *DataCleaning.R* is used to compute the data transformations and division and stores them in the *data* folder.
- *ChainsAnalysis.R* is used to compute the various statistics we want to compute from a model, using the chains saved in *chains* folder.
- *dataCorrelationAnalysis.R* is used to graphically explore the correlations that exists between the dataset features.
- *PosteriorMCMC_Analysis.R* is used to cunduct an analysis on the posterior densities of the parameter vectors obtained from the JAGS model.
- *MCMCdiagnostic.R* contains a detailed diagnostic regarding the mcmc chains convergence.

Authors:

Gabriele Curti, Samuele Mariani, Alessandro Molteni, Matteo Monti
