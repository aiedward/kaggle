# author: hercky
# solution to kaggle's digit recognizer challenge using mlp library
# references: 
# http://www.inside-r.org/packages/cran/RSNNS/docs/mlp

library(caTools)
library(ggplot2)
library(RSNNS)

# read the data and spilt into learning and cross-validation sets
train = read.csv('./data/train.csv', header=TRUE)
test = read.csv('./data/test.csv' , header=TRUE)

# shuffle , I'm only taking a subset of data here as it takes too much time 
train <- train[sample(1:nrow(train),(nrow(train)/7)),1:ncol(train)]

# convert label into binary variables
tr_target <- decodeClassLabels(train[,1])

# split the training set into learning and cross-validation
train <- splitForTrainingAndTest(train,tr_target,ratio=0.3)

# normalize the data, to make it faster
train <- normTrainingAndTestSet(train,type="0_1")
test <- normalizeData(test,type="0_1")

# run the mlp
model <- mlp(train$inputsTrain ,train$targetsTrain,
            size=5, learnFuncParams = c(0.1), maxit=100,
            inputsTest=train$inputsTest , targetsTest = train$targetsTest )

# make the prediction
prediction <- predict(model,test)

head(prediction)

# cross-validations via confusion matrix
# can also explore plotIterativeError, plotRegressionError, plotROC 
confusionMatrix(train$targetsTrain,model$fitted.values)
confusionMatrix(train$targetsTest,model$fittedTestValues)


# output the label which has the maximum value for an input
result <- apply(prediction,1,which.max)