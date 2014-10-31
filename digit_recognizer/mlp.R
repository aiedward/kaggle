library(caTools)
library(ggplot2)
library(RSNNS)


train = read.csv('./data/train.csv', header=TRUE)
test = read.csv('./data/test.csv' , header=TRUE)

train <- train[sample(1:nrow(train),(nrow(train)/7)),1:ncol(train)]

tr_target <- decodeClassLabels(train[,1])

train <- splitForTrainingAndTest(train,tr_target,ratio=0.3)

train <- normTrainingAndTestSet(train,type="0_1")
test <- normalizeData(test,type="0_1")

model <- mlp(train$inputsTrain ,train$targetsTrain,
            size=5, learnFuncParams = c(0.1), maxit=100,
            inputsTest=train$inputsTest , targetsTest = train$targetsTest )
