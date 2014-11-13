library(caTools)
library(ggplot2)
library(caret)
library(doSNOW)
# not using the below 
# library(e1071)
# library(kernlab)

# Function to randomly split the data
splitdf <- function(dataframe, nSplits = 2, seed=NULL) {
    if (!is.null(seed)) set.seed(seed)
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/nSplits))
    trainset <- dataframe[-trainindex, ]
    validationset <- dataframe[trainindex, ]
    list(trainset=trainset,validationset=validationset)
}

train = read.csv('./data/train.csv', header=TRUE)
test = read.csv('./data/test.csv' , header=TRUE)

## only look at first 1000 observations
smallTest <- test[1:1000,]
smallTrain <- train[1:1000,]  

# create the validation set by splitting the training set up into 66% / 33%
splits <- splitdf(smallTrain, nSplits=3, seed=808)
trainingSet <- splits$trainset
validationSet  <- splits$validationset

# take the first column of the training/validation set for y values, the rest for x values
yTrainingLabels <- as.factor(trainingSet[,1])
xTrain <- trainingSet[,-1]
yValLabels <- as.factor(validationSet[,1])
xVal <- validationSet[,-1]
yFullTrainingLabels <- as.factor(train[,1])
xFullTrain <- train[,-1]


# Remove columns with zero variance
zero_var_test  <- nearZeroVar(smallTest)
zero_var_train <- nearZeroVar(smallTrain)
xTrain    <- xTrain[,-zero_var_train]
xVal      <- xVal[,-zero_var_train]
xTrainFull <- xFullTrain[,-zero_var_train]
TestFull   <- test[,-zero_var_train]


#Parallize
nCores <- 2
c1 <- makeCluster(nCores, type = "SOCK")
registerDoSNOW(c1) 
clusterExport(c1, c("%do%","foreach"))

## load libraries on workers
clusterEvalQ(c1, library(caret)) 

tic=proc.time()[3]

rdaGrid_Poly = data.frame(.C=1, .degree=3, .scale=1e-07)

ctrl_Poly <- trainControl(method = "repeatedcv",
                          repeats = 3,
                          classProbs = TRUE
)

model <- train(xVal, yValLabels, 
               method='svmPoly',
               #tuneGrid = rdaGrid_Poly,
               trControl = ctrl_Poly,
               tuneLength = 2,
               #metric = "ROC"
)


ctrl_Radial <- trainControl(method = "cv",
                            number = 5,
                            classProbs = TRUE
)

# Find a good Sigma 
model <- train(xVal, yValLabels, 
               method='svmRadial',
               #trControl = ctrl_Radial,
               trControl = ctrl_Poly,
               tuneLength = 5
              )

Tuning parameter 'sigma' was held constant at a value of 1.767154e-07
Accuracy was used to select the optimal model using  the largest value.
The final values used for the model were sigma = 1.767154e-07 and C = 4.

model <- train(xVal, yValLabels, 
               method='svmRadial',
               trControl = ctrl_Radial,
               tuneGrid = expand.grid(.sigma=c(2.88e-07),.C=c(1,2,4,8,16,32))
              )


Tuning parameter 'sigma' was held constant at a value of 2.88e-07
Accuracy was used to select the optimal model using  the largest value.
The final values used for the model were sigma = 2.88e-07 and C = 16. 


###
###   FIRST FULL TRAINING
###

tic=proc.time()[3]
# Full training example
model <- train(xTrainFull, yFullTrainingLabels, 
               method='svmRadial',
               tuneGrid = expand.grid(.sigma=c(2.94e-07),.C=c(2))
              )

toc=proc.time()[3] - tic
toc

print(model)
plot(model)


predY <- predict(model, TestFull)
predY

predictions <- levels(yTrainingLabels)[predY]
predictions

write(predictions, file="./svm_benchmark.csv", 
      ncolumns=1) 

# imp !! at end stop the  cluster 
stopCluster(c1)