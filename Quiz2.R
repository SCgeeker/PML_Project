# Q1 
library(AppliedPredictiveModeling); library(caret); data(AlzheimerDisease)
## Code 01 
adData = data.frame(id=paste0('p',1:333), diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]
sum(training[,1]==testing[,1])
## Code 02   not include diagnosis labels
adData = data.frame(id=paste0('p',1:333),predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
sum(training[,1]==testing[,1])
## Code 03  assgin the same sample
adData = data.frame(id=paste0('p',1:333), diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[trainIndex,]
sum(training[,1]==testing[,1])
## Code 04  assgin the same sample
adData = data.frame(id=paste0('p',1:333), diagnosis,predictors)
train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)
sum(training[,1]==testing[,1])





set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#Q4
## Find all the predictor variables in the training set that begin with IL. 
## Perform principal components on these variables with the preProcess() function from the caret package. 
## Calculate the number of principal components needed to capture 90% of the variance. 
## How many are there?
trainingIL <- training[,grep("^IL", names(training) )  ]
testingIL <-testing[,grep("^IL", names(testing) )  ]


preProc5 <- preProcess(trainingIL, method="pca", pcaComp=5)
preProc7 <- preProcess(trainingIL, method="pca", pcaComp=7)
preProc9 <- preProcess(trainingIL, method="pca", pcaComp=9)
preProc11 <- preProcess(trainingIL, method="pca", pcaComp=11)

trainPC5 <- predict(preProc5,trainingIL)
trainPC7 <- predict(preProc7,trainingIL)
trainPC9 <- predict(preProc9,trainingIL)
trainPC11 <- predict(preProc11,trainingIL)

modelFit5 <- train(training$diagnosis ~ .,method="glm",data=trainPC5)
modelFit7 <- train(training$diagnosis ~ .,method="glm",data=trainPC7)
modelFit9 <- train(training$diagnosis ~ .,method="glm",data=trainPC9)
modelFit11 <- train(training$diagnosis ~ .,method="glm",data=trainPC11)

modelFit5$finalModel
modelFit7$finalModel
modelFit9$finalModel
modelFit11$finalModel

testPC5 <- predict(preProc5,testingIL)
testPC7 <- predict(preProc7,testingIL)
testPC9 <- predict(preProc9,testingIL)
testPC11 <- predict(preProc11,testingIL)

confusionMatrix(testing$diagnosis, predict(modelFit5,testPC5))
confusionMatrix(testing$diagnosis, predict(modelFit7,testPC7))
confusionMatrix(testing$diagnosis, predict(modelFit9,testPC9))
confusionMatrix(testing$diagnosis, predict(modelFit11,testPC11))



#Q5
## Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis.
## Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining
## 80% of the variance in the predictors. Use method="glm" in the train function.
## What is the accuracy of each method in the test set? Which is more accurate?





library(AppliedPredictiveModeling); data(concrete); library(caret); library(Hmisc); library(gridExtra)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

#Q2
dim(training); dim(testing)
summary(training);summary(testing)

## Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
## Color by each of the variables in the data set 
## (you may find the cut2() function in the Hmisc package useful for turning continuous covariates into factors). 
## What do you notice in these plots?
cutCS <- cut2(training$CompressiveStrength, g=3)
table(cutCS)

p1 <- qplot(cutCS, Cement, data=training, fill=cutCS, geom=c("boxplot"))
p2 <- qplot(cutCS, BlastFurnaceSlag, data=training, fill=cutCS, geom=c("boxplot"))
p3 <- qplot(cutCS, FlyAsh, data=training, fill=cutCS, geom=c("boxplot"))
p4 <- qplot(cutCS, Water, data=training, fill=cutCS, geom=c("boxplot"))
p5 <- qplot(cutCS, Superplasticizer, data=training, fill=cutCS, geom=c("boxplot"))
p6 <- qplot(cutCS, CoarseAggregate, data=training, fill=cutCS, geom=c("boxplot"))
p7 <- qplot(cutCS, FineAggregate, data=training, fill=cutCS, geom=c("boxplot"))
p8 <- qplot(cutCS, Age, data=training, fill=cutCS, geom=c("boxplot"))
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=4,nrow=2)

#Q3
## Make a histogram and confirm the SuperPlasticizer variable is skewed. 
## Normally you might use the log transform to try to make the data more symmetric. 
## Why would that be a poor choice for this variable?
hist(log( training$Superplasticizer + 1 ) )


