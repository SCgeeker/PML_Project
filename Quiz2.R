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
preProc <- preProcess(training[,grep("^IL", names(training) )], method="pca", pcaComp=12)
# adPC <- predict(preProc, training[,grep("^IL", names(training) )])
trainAD <- predict(preProc, training[,grep("^IL", names(training) )])
modelFit <- train(training$diagnosis ~ names(training)[grep("^IL", names(training) )], method=glm, data=training)


modelFit <- train

#Q5
## 