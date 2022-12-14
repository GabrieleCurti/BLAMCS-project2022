library('rjags')

rm(list=ls())

# Load train and test
load("data/ford_train.dat")
load("data/ford_test.dat")
yp <- test$price

load("chains/allCovariatesNoOut/predictionOnTest.dat")
matrix <- as.matrix(predictionsTestMCMC)

computeSigma <- function(betaDataString) {
  load(betaDataString)
  allMatrix <- as.matrix(betasMCMC)
  print(mean(allMatrix[,"sigma"]))
}


computeR2 <- function(betaDataString) {
  load(betaDataString)
  allMatrix <- as.matrix(betasMCMC)
  print(mean(allMatrix[,"R2"]))
}

computeTestRMSE <- function(predictionFileString, yp) {
  load(predictionFileString)
  allMatrix <- as.matrix(predictionsTestMCMC)
  yPred <- colMeans(allMatrix)
  rmse <- sqrt(mean((yPred-yp)^2))
  print(rmse)
  print((rmse/mean(test$price))*100)
}

computeTrainRMSE <- function(predictionFileString, y) {
  load(predictionFileString)
  allMatrix <- as.matrix(predictionsTrainMCMC)
  yPred <- colMeans(allMatrix)
  rmse <- sqrt(mean((yPred-y)^2))
  print(rmse)
  print((rmse/mean(train$price))*100)
}

computeConfidenceInterval <- function(predictionFileString, dataPoint) {
  load(predictionFileString)
  allMatrix <- as.matrix(predictionsTestMCMC)
  predictions <- allMatrix[,dataPoint]
  probs <- c(0.05, 0.95)
  print(mean(predictions))
  quantile(predictions, probs)
}

computeConfidenceInterval("chains/spikeNSlab5NoOut/predictionOnTest.dat", "yp[1]")
computeConfidenceInterval("chains/spikeNSlab5NoOut/predictionOnTest.dat", "yp[2]")
computeConfidenceInterval("chains/spikeNSlab5NoOut/predictionOnTest.dat", "yp[3]")

computeR2("chains/allCovariatesNoOut/betasAndStuff.dat")
computeR2("chains/basSelectionNoOut/betasAndStuff.dat")
computeR2("chains/spikeNSlab5NoOut/betasAndStuff.dat")
computeR2("chains/spikeNSlab6NoOut/betasAndStuff.dat")

computeSigma("chains/allCovariatesNoOut/betasAndStuff.dat")
computeSigma("chains/basSelectionNoOut/betasAndStuff.dat")
computeSigma("chains/spikeNSlab5NoOut/betasAndStuff.dat")
computeSigma("chains/spikeNSlab6NoOut/betasAndStuff.dat")

computeSigma("chains/allCovariates/betasAndStuff.dat")
computeSigma("chains/basSelection/betasAndStuff.dat")
computeSigma("chains/spikeNSlab5/betasAndStuff.dat")
computeSigma("chains/spikeNSlab6/betasAndStuff.dat")


computeTestRMSE("chains/allCovariatesNoOut/predictionOnTest.dat",test$price)
computeTestRMSE("chains/spikeNSlab5NoOut/predictionOnTest.dat", test$price)
computeTestRMSE("chains/spikeNSlab6NoOut/predictionOnTest.dat", test$price)

computeTrainRMSE("chains/allCovariatesNoOut/predictionOnTrain.dat",train$price)
computeTrainRMSE("chains/spikeNSlab5NoOut/predictionOnTrain.dat", train$price)
computeTrainRMSE("chains/spikeNSlab6NoOut/predictionOnTrain.dat", train$price)

computeR2("chains/basSelectionNoOut/betasAndStuff.dat")
computeTestRMSE("chains/basSelectionNoOut/predictionOnTest.dat", test$price)
computeTrainRMSE("chains/basSelectionNoOut/predictionOnTrain.dat", train$price)


computeR2("chains/allCovariates/betasAndStuff.dat")
computeR2("chains/basSelection/betasAndStuff.dat")
computeR2("chains/spikeNSlab5/betasAndStuff.dat")
computeR2("chains/spikeNSlab6/betasAndStuff.dat")

computeTestRMSE("chains/allCovariates/predictionOnTest.dat",test$price)
computeTestRMSE("chains/basSelection/predictionOnTest.dat", test$price)
computeTestRMSE("chains/spikeNSlab5/predictionOnTest.dat", test$price)
computeTestRMSE("chains/spikeNSlab6/predictionOnTest.dat", test$price)

computeTrainRMSE("chains/allCovariates/predictionOnTrain.dat",train$price)
computeTrainRMSE("chains/basSelection/predictionOnTrain.dat", train$price)
computeTrainRMSE("chains/spikeNSlab5/predictionOnTrain.dat", train$price)
computeTrainRMSE("chains/spikeNSlab6/predictionOnTrain.dat", train$price)

computeSigma("chains/basSelection/betasAndStuff.dat")

