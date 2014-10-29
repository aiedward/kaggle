library(caTools)
library(ggplot2)
library(neuralnet)
library(RSNNS)
library(nnet)

train = read.csv('./data/train.csv', header=TRUE)
test = read.csv('./data/test.csv' , header=TRUE)

split = sample.split(train$label, SplitRatio=.75)

tl = subset(train, split==TRUE)
tcv = subset(train, split==FALSE)

eqn <- paste('label1 + label2 + label3 + 
             label4 + label5 + 
             label6 + label7 + 
             label8 + label9 ~ ',
             paste(paste('pixel',0:783,sep=''), collapse='+'),
             sep='')

tmp <- tl[1:(0.3*nrow(tl)),]
tmp$label <- as.factor(tmp$label)

str(tmp)

m <- model.matrix( 
    formula(paste('~ label + ',paste(paste('pixel',0:783,sep=''), collapse=' + '), sep='')) , 
    data = tmp
)

head(m)

model <- neuralnet(eqn,
                   data=m, 
                   hidden=10, 
                   threshold=0.1, 
                   rep=1, 
                   lifesign="full",
                   stepmax = 10000)

# plot the model 
# plot(model)

tmp_tcv <- tcv
tmp_tcv$label <- NULL
cvr <- compute(model,tmp_tcv)
ls(cvr)