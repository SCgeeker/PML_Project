#Q1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

## 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
Training <- segmentationOriginal[segmentationOriginal$Case == 'Train',][,-2]
Testing <- segmentationOriginal[segmentationOriginal$Case == 'Test',][,-2]
## 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. 
set.seed(125)
modFit <- train(Class ~ . ,method="rpart" ,data=Training)
## 3. In the final model what would be the final model prediction for cases with the following variable values:
print(modFit$finalModel)
library(rattle)
fancyRpartPlot(modFit$finalModel)
##  a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2  ### PS
##  b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100  ### WS
##  c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100   ### PS
##  d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2         ### Not possible to predict




#Q2
## If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger? 
## If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. 

### Mentioned in Lecture 008 cross Validation
### Larger k = less bias, more variance; Small k = more bias, less variance

## Is K large or small in leave one out cross validation?

### Image each sample could be tested in each cycle of model fitting.


#Q3
library(pgmm)
data(olive)
olive = olive[,-1]

## (NOTE: If you have trouble installing the pgmm package, you can download the olive dataset here: olive_data.zip. After unzipping the archive, you can load the file using the load() function in R.)
## These data contain information on 572 different Italian olive oils from multiple regions in Italy. 
## Fit a classification tree where Area is the outcome variable. 

require(caret)
inTrain <- createDataPartition(y=olive$Area,
                              p=0.75, list=FALSE)
trainOlive <- olive[inTrain,]
testOlive <- olive[-inTrain,]
dim(trainOlive); dim(testOlive)
oliveFit <- train(Area ~ .,method="rpart",data=olive)

## Then predict the value of area for the following data frame using the tree command with all defaults

newdata = as.data.frame(t(colMeans(olive)))

newdata[,1]
predict(oliveFit,newdata)   ### It is not like the given answer.

## What is the resulting prediction? Is the resulting prediction strange? Why or why not?




#Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

## Then set the seed to 13234 and fit a logistic regression model (method="glm", be sure to specify family="binomial") 

set.seed(13234)
modFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,data=trainSA,method="glm", family="binomial")
summary(modFit$finalModel)

## with Coronary Heart Disease (chd) as the outcome and age at onset, current alcohol consumption, obesity levels, 
## cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors. 
## Calculate the misclassification rate for your model using this function and a prediction on the "response" scale:

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

## What is the misclassification rate on the training set? What is the misclassification rate on the test set?

missClass(testSA$chd, predict(modFit, testSA)) ### the misclassification rate on the test set
missClass(trainSA$chd, predict(modFit, trainSA)) ###  the misclassification rate on the training set



#Q5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

## Set the variable y to be a factor variable in both the training and test set. 
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

## Then set the seed to 33833. Fit a random forest predictor relating the factor variable y to the remaining variables.
set.seed(33833)
require(caret)
modFit <- train(y~ .,data=vowel.train,method="rf")
modFit

## Read about variable importance in random forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr 
## The caret package uses by defualt the Gini importance.
## Calculate the variable importance using the varImp function in the caret package.
## What is the order of variable importance?

varImp(modFit)  ### It is not like the given order.
