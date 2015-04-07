# 

train <- read.csv("~/code/kaggle/stage/data/train.csv")

# convert date to age and log transform
train$Open.Date <- as.Date(train$Open.Date , format = "%m/%d/%Y")
train$Age <- log(as.numeric(Sys.Date() - train$Open.Date))
train$Open.Date <- NULL

train$Id <- NULL


# convert factor to binary vector
dummyCity <- model.matrix(~City,data=train)[,-1]
dummyCity.Group <- model.matrix(~City.Group,data=train)[,-1]
dummyType <- model.matrix(~Type,data=train)[,-1]
Filter(is.numeric ,train)
train <- Filter(is.numeric ,train)
train <- cbind(train,dummyCity,dummyCity.Group,dummyType)

#fit linear regression
fit <- lm( revenue ~ . , data = train)
summary(fit)
anova(fit)

# 10 fold cross validation
cv.lm(df = train, fit,m = 10)
cvlm <- cv.lm(df = train, fit,m = 10)

# test file
test <- read.csv("~/code/kaggle/stage/data/test.csv")
# convert date to age and log transform
test$Open.Date <- as.Date(test$Open.Date , format = "%m/%d/%Y")
test$Age <- log(as.numeric(Sys.Date() - test$Open.Date))
test$Open.Date <- NULL
test$Id <- NULL
fit <- lm( revenue ~ . , data = train)
fit <- lm( revenue ~ . , data = train)
test$City <- NULL
r_id <- seq(0,nrow(test)-1)
test$Type[test$Type == 'MB'] <- 'DT'
r_score <- predict(fit,test)
cbind(r_id,r_score)
write.csv(cbind(r_id,r_score),file="test_submission.csv",row.names= FALSE)


test$Open.Date <- as.Date(test$Open.Date , format = "%m/%d/%Y")
test$Age <- log(as.numeric(Sys.Date() - test$Open.Date))
test$Open.Date <- NULL

