# author: hercky
# solution to kaggle's digit recognizer challenge using a neuralnet library
# references: 
# http://www.di.fc.ul.pt/~jpn/r/neuralnets/neuralnets.html
# https://beckmw.wordpress.com/tag/neural-network/

library(caTools)
library(ggplot2)
library(neuralnet)
# we only we'll be using neyaralnet in this case
# library(RSNNS)
# library(nnet)


# read the data and spilt into learning and cross-validation sets
train = read.csv('./data/train.csv', header=TRUE)
test = read.csv('./data/test.csv' , header=TRUE)

split = sample.split(train$label, SplitRatio=.75)

tl = subset(train, split==TRUE)
tcv = subset(train, split==FALSE)

# convert the digit label from into to factor
tmp <- tl[1:(0.3*nrow(tl)),]
tmp$label <- as.factor(tmp$label)

str(tmp)

# Since neuralnet only deals with quantitative variables, we have to convert 
# all the qualitative variables (factors) to binary ("dummy") variables,
# with the model.matrix function (labels in this case)

m <- model.matrix( 
    formula(paste('~ label + ',paste(paste('pixel',0:783,sep=''), collapse=' + '), sep='')) , 
    data = tmp
)

head(m)

# the new formula
eqn <- paste('label1 + label2 + label3 + 
             label4 + label5 + 
             label6 + label7 + 
             label8 + label9 ~ ',
             paste(paste('pixel',0:783,sep=''), collapse='+'),
             sep='')

# it runs too slow
# not the actual arguments provided here
# for better result reduce threshold, use better stepmax
model <- neuralnet(eqn,
                   data=m, 
                   hidden=10, 
                   threshold=0.1, 
                   rep=50, 
                   lifesign="full",
                   stepmax = 10000)

# plot the model 
# plot(model)

# have to remove label columns from the test data
tmp_tcv <- tcv
tmp_tcv$label <- NULL

cvr <- compute(model,tmp_tcv)
ls(cvr)