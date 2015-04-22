# Load required
setwd("F:/Data_science_programs/PracticalMachineLearning/PML_Projects")
source("./Writeup/Loaddata.R")
source("./Writeup/pml_write_files.R")

#Load caret package
library(class) # use KNN 
library(kernlab) # use SVM
library(caret) # use many algorithm
library(party)
library(RANN)

# Clean method 1: Label all NA variables
TrainCleaned1 <- data.frame(classe = pml_training[,159], pml_training[,-c(1:6,159)])
naVAR <- ( 1:length(names(TrainCleaned1[,-1]) ))[is.na( apply(TrainCleaned1[,-1], 2, sum) )] + 1

# method1: pre-processing missing data
preTrainCleaned1 <- preProcess(TrainCleaned1[,-1], method="knnImpute")
avgTrainCleaned1 <- predict(preTrainCleaned1, TrainCleaned1[,-1])
## Some variables have zero variance.

TrainCleaned1[ ,naVAR] <- avg
preTrainCleaned1

TrainCleaned2$avg_roll_belt <- avgTrainCleaned2$avg_roll_belt
TrainCleaned1 <- data.frame(classe = pml_training[,159], pml_training[,-c(1:6,159)][,!is.na( apply(pml_training[,-c(1:6,159)], 2, sum) )])
TestCleaned1 <- data.frame(id = pml_testing[,159], pml_testing[,-c(1:6,159)][,!is.na( apply(pml_testing[,-c(1:6,159)], 2, sum) )])


# Clean method 2: take variables author selected
AuthorSelected <- c("avg_roll_belt", "var_roll_belt", "total_accel_belt","var_total_accel_belt", "accel_belt_x", "accel_belt_y", "accel_belt_z", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "magnet_belt_x", "magnet_belt_y", "magnet_belt_z","accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x", "magnet_arm_y", "magnet_arm_z","accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z", "gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z", "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z", "pitch_forearm", "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z")
TrainCleaned2 <- data.frame(classe = pml_training[,159], pml_training[,AuthorSelected])
TestCleaned2 <- data.frame(classe = pml_testing[,159], pml_testing[,AuthorSelected])

# Check missing value
head(TrainCleaned2)

# method2: pre-processing missing data
preTrainCleaned2 <- preProcess(TrainCleaned2[,-1], method="knnImpute")
avgTrainCleaned2 <- predict(preTrainCleaned2, TrainCleaned2[,-1])


# impute "avg_roll_belt"  "var_roll_belt"  "var_total_accel_belt" in Train and Test data set.

TrainCleaned2$avg_roll_belt <- avgTrainCleaned2$avg_roll_belt
TrainCleaned2$var_roll_belt <- avgTrainCleaned2$var_roll_belt
TrainCleaned2$var_total_accel_belt <- avgTrainCleaned2$var_total_accel_belt

head(TrainCleaned2)

TestCleaned2$avg_roll_belt <- 0
TestCleaned2$var_roll_belt <- 0
TestCleaned2$var_total_accel_belt <- 0
  
# k-Nearest Neighbour Classification
knnPred <- matrix("ans", 20, 20)
knnPredProb <- matrix(0, 20, 20)
for(i in 1:20)
{
  knnPred[,i] <- as.character( knn(TrainCleaned2[,-1], TestCleaned2[,-1], TrainCleaned2$classe, k=i, prob=TRUE ) ) 
    knnPredProb[,i] <- attributes( knn(TrainCleaned2[,-1], TestCleaned2[,-1], TrainCleaned2$classe, k=i, prob=TRUE ) )$prob
}
# problem id=1,8,11, has two possible answers(A or B)

apply(knnPredProb, 2, mean)

plot(1:20, apply(knnPredProb, 2, min), ylim=0:1, type="b", col=8, xlab = "number of neighbours(K)", ylab = "Proportion of the votes for the winner", main = "Average proportion and the smallest proportion for per K")
lines(1:20, apply(knnPredProb, 2, mean), type="b", col=6)
# This is why I use K=5 as my answers


# Tree prediction
treeFit <- train(classe ~ ., method = "rpart", data = TrainCleaned2)

print(treeFit$finalModel)
plot(treeFit$finalModel, uniform = TRUE)
text(treeFit$finalModel,use.n = TRUE, all = TRUE)

predict(treeFit, TestCleaned2[,-1])
# predictions are far from KNN


# GBM prediction
gbmFIT <-  train(classe ~ ., method = "gbm", data = TrainCleaned2)
print(gbmFIT)
#round(table(predict(gbmFIT, TrainCleaned2), TrainCleaned2[,1])/summary(TrainCleaned2[,1]), digits=2)
confusionMatrix( TrainCleaned2$classe,   predict(gbmFIT, TrainCleaned2) ) 

gbmANS <- predict(gbmFIT, TestCleaned2)


# ans = rep("A", 20)
# Write into answer files
pml_write_files(knnPred[,5])
