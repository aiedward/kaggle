# author: hercky
# solution to kaggle's digit recognizer challenge using a random forest ensemble
# refernce: http://akshayluther.com/2013/03/13/amateur-data-scientist-how-i-built-a-handwritten-digit-recognizer-with-95-accuracy/

# load the libraries to get stuff done 
library(caTools)
library(rpart)
library(rattle)
library(RColorBrewer)
library(ggplot2)
library(randomForest)

# read the data and spilt into learning and cross-validation sets
train = read.csv('./data/train.csv', header=TRUE)
test = read.csv('./data/test.csv' , header=TRUE)
split = sample.split(train$label, SplitRatio=.75)
tl = subset(train, split==TRUE)
tcv = subset(train, split==FALSE)

## alternatively can split aslo via
#
# tid <- sample(train, nrow(train)*0.75)
# tl <- subset(train, tid %in% train)
# tcv <- subset(train, !(tid %in% train))

# label vs the rest (pixels in this case)
eqn <- formula(label ~ .)

# train a decision tree
model = rpart(eqn, data=tl, 
              method="class", 
              control = rpart.control(minsplit=30))
# view the dcision tree 
fancyRpartPlot(model)

prediction = predict(model, newdata=tcv, type="class")

# cross-validate the result
# calculate the number of labels of each kind predicted by the trained 
# model against actual data
# cvTable = table(tl$label, prediction)
# sum(diag(cvTable))/nrow(tcv)

tl$label = factor(tl$label)
tcv$label = factor(tcv$label)

# build random forest ensemble
set.seed(666)
model = randomForest(eqn, data=tl, 
                     nodesize=10, 
                     ntree=100, 
                     do.trace=TRUE, 
                     importance=TRUE)
prediction = predict(model, newdata=tcv)


# if the accuracy staisfies you 
# run on the actual test data

prediction = predict(model, newdata=test)

# manually add the column heading - ImageId,Label
# yes I'm that lazy
write.csv(prediction, 'prediction.csv',quote=FALSE)