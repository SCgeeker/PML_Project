#Q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
## Set the variable y to be a factor variable in both the training and test set. 
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

## Then set the seed to 33833. 
set.seed(33833)

## Fit (1) a random forest predictor relating the factor variable y to the remaining variables and 
require(caret)
## (2) a boosted predictor using the "gbm" method. 
## Fit these both with the train() command in the caret package. 
vowelRf <- train(y~ .,data=vowel.train,method="rf")
vowelGbm <- train(y~ .,data=vowel.train,method="gbm")

## What are the accuracies for the two approaches on the test data set? 
## What is the accuracy among the test set samples where the two methods agree?
predRf <- predict(vowelRf,vowel.test); predGbm <- predict(vowelGbm,vowel.test)
confusionMatrix(vowel.test$y, predRf)$overall[1]
confusionMatrix(vowel.test$y, predGbm)$overall[1]
predDF <- data.frame(predRf,predGbm,y=vowel.test$y,agree=(predRf == predGbm)) ## According to the lecture "Combining predictors", the two models generate identical predicted values.
accuracy <- sum(predDF$predRf[predDF$agree] == predDF$y[predDF$agree])/sum(predDF$agree)  # close to the option



#Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

## Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model.
set.seed(62433)
modRf <- train(diagnosis~ ., data = training, method="rf", trControl=trainControl(number=3))
modGbm <- train(diagnosis~ ., data = training, method="gbm")
modLda <- train(diagnosis~ ., data = training, method="lda")
predRf <- predict(modRf,testing)
predGbm <- predict(modGbm,testing)
predLda <- predict(modLda,testing)

## Stack the predictions together using random forests ("rf").
predDF <- data.frame(predRf,predGbm,predLda,diagnosis=testing$diagnosis)
combModFit <- train(diagnosis ~.,predDF,method="rf")
combPred <- predict(combModFit,testing)
## What is the resulting accuracy on the test set?
confusionMatrix(combPred, testing$diagnosis)$overall[1] # this is a little larger than boosted tree
## Is it better or worse than each of the individual predictions?
confusionMatrix(predRf, testing$diagnosis)$overall[1]
confusionMatrix(predGbm, testing$diagnosis)$overall[1]
confusionMatrix(predLda, testing$diagnosis)$overall[1]


#Q3
set.seed(3523)
library(caret)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

## Set the seed to 233 and fit a lasso model to predict Compressive Strength.
set.seed(233)
modLasso <- train(CompressiveStrength~., data=training, method="lasso")
#predLasso <- predict(modLasso, testing)
## Which variable is the last coefficient to be set to zero as the penalty increases?
## (Hint: it may be useful to look up ?plot.enet).
require(MASS)
covnames <- names(training)[-9]
lambdas <- seq(0,30000,len=10)
M <- length(lambdas)
betas <- matrix(0,ncol(training)-1,M)
for(i in 1:M){
    fit1 <- lm.ridge(modLasso,data=training,lambda=lambdas[i])
    betas[,i] <- fit1$coef
}

plot(lambdas,betas[1,],ylim=range(betas),type="n",ylab="Coefficients")
for(i in 1:length(lambdas))
  lines(lambdas,betas[i,],type="b",lty=i,pch=as.character(i))
abline(h=0)
legend(3,12,covnames,pch=as.character(1:8))

plot.enet(modLasso$finalModel, xvar="penalty", use.color=TRUE)

#Q4
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
## Fit a model using the bats() function in the forecast package to the training time series.
require(forecast)
Tumblr.fit <- bats(tstrain)
## Then forecast this model for the remaining time points.
## For how many of the testing points is the true value within the 95% prediction interval bounds?
Tumblr.fcast <- forecast(Tumblr.fit, level=95, h=nrow(testing))
sum(testing$visitsTumblr < Tumblr.fcast$upper)/length(testing$visitsTumblr)



#Q5
library(caret)
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

## Set the seed to 325 and fit a support vector machine using the e1071 package 
## to predict Compressive Strength using the default settings.
set.seed(325)
library(e1071)
modFit <- svm(CompressiveStrength ~ ., data=training)
## Predict on the testing set. What is the RMSE?
pred <- predict(modFit, testing)
sqrt( (1/nrow(testing)) * sum( (pred - testing$CompressiveStrength)^2 ) ) # a littler larger than the option
