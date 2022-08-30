library('rjags')

rm(list=ls())

# Load train and test
load("data/ford_train.dat")
load("data/ford_test.dat")
yp <- test$price

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

