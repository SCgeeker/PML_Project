# Load required
setwd("F:/Data_science_programs/PracticalMachineLearning/PML_Projects")
source("./Writeup/Loaddata.R")
source("./Writeup/pml_write_files.R")


#Load caret package
library(caret)
library(party)

# Explorary analysis and clean data
dim(pml_training)
## 19622   159
#  summary(pml_training)
table(pml_training$user_name, pml_training$classe)

## Total 152 variables in addition to "classe", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window"

# check x, y z features
#summary(pml_training[,c("gyros_forearm_x","gyros_forearm_y","gyros_forearm_z")])
#summary(pml_training[,c("gyros_dumbbell_x","gyros_dumbbell_y","gyros_dumbbell_z")])
#summary(pml_training[,c("gyros_belt_x","gyros_belt_y","gyros_belt_z")])

#summary(pml_training[,c(1:6,159)])
#summary(pml_training[,-c(1:6,159)])

#dimnames( pml_training[,-c(1:6,159)][,!is.na( apply(pml_training[,-c(1:6,159)], 2, sum) )] )[[2]]
#summary( pml_training[,-c(1:6,159)][,!is.na( apply(pml_training[,-c(1:6,159)], 2, sum) )])

# Clean method 1: exclude all NA variables
TrainCleaned1 <- data.frame(classe = pml_training[,159], pml_training[,-c(1:6,159)][,!is.na( apply(pml_training[,-c(1:6,159)], 2, sum) )])

# Clean method 2: take variables author selected
AuthorSelected <- c("avg_roll_belt", "var_roll_belt", "total_accel_belt","var_total_accel_belt", "accel_belt_x", "accel_belt_y", "accel_belt_z", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "magnet_belt_x", "magnet_belt_y", "magnet_belt_z","accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x", "magnet_arm_y", "magnet_arm_z","accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z", "gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z", "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z", "pitch_forearm", "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z")
TrainCleaned2 <- data.frame(classe = pml_training[,159], pml_training[,AuthorSelected])


dim(pml_testing)
## 20 159
#  summary(pml_testing)
#  cbind(as.character(pml_testing$user_name), pml_testing$problem_id)
#  table(as.character(pml_testing$user_name), pml_testing$problem_id)

# overview variable names
#names(pml_training[,grep("_belt",names(pml_training))])


#vind <- c( grep("roll_belt",names(pml_training)), grep("accel_belt",names(pml_training)), 
#           grep("gyros_belt",names(pml_training)),grep("magnet_belt",names(pml_training)),
#           grep("accel_arm",names(pml_training)), grep("magnet_arm",names(pml_training)),
#           grep("accel_dumbbell",names(pml_training)), grep("gyros_dumbbell",names(pml_training)),
#           grep("magnet_dumbbell",names(pml_training)),grep("gyros_forearm",names(pml_training)),
#           grep("pitch_forearm",names(pml_training))
#)

TestCleaned1 <- data.frame(id = pml_testing[,159], pml_testing[,-c(1:6,159)][,!is.na( apply(pml_testing[,-c(1:6,159)], 2, sum) )])

TestCleaned2 <- data.frame(classe = pml_testing[,159], pml_testing[,AuthorSelected])

# Available training data set
tmp = pml_training$classe
levels(tmp) <- c(1:5)


#pml_training1 <- data.frame( pml_training[pml_training$new_window=="no",vind], classe = tmp[pml_training$new_window=="no"])

# Pre-Processing in use of PCA
preProc17 <- preProcess(TrainCleaned[,-1], method = "pca", pcaComp = 17)

# Find a fitted model
TrainPC17 <- predict(preProc17,TrainCleaned[,-1])

modelFit17 <- train(as.numeric(TrainCleaned$classe) ~ ., method="glm",data=TrainPC17)

# List predictions of this fitted model
modelOUT17 <- predict(modelFit17, TrainPC17)

# Test Classifier Rule
rule_pml <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x < 1.5] <- "A"
  prediction[(x >= 1.5 & x < 2.5)] <- "B"
  prediction[(x >= 2.5 & x < 3.5)] <- "C"
  prediction[(x >= 3.5 & x < 4.5)] <- "D"
  prediction[x >= 4.5] <- "E"
  return(prediction)
}

confusionMatrix( TrainCleaned$classe,  as.factor( rule_pml(modelOUT17) ) ) 
length(TrainCleaned$classe)# Model OUT had 0 and 6; Not match true classe


# Use the model on the test cases
TestPC17 <- predict(preProc17,TestCleaned[,-1])
predict(modelFit17, TestPC17)

confusionMatrix(TestCleaned$id,predict(modelFit17,TestPC17))

# Make predict values in use of bagging
predictors2 = TrainCleaned2[,-1]
classe2 = as.numeric( TrainCleaned2$classe )
treebag2 <- bag(predictors2, classe2, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))  # check package 'party'
treebag2PC <- predict(treebag2, predictors2)

treebag2_eval <- confusionMatrix( TrainCleaned2$classe,  as.factor( rule_pml(treebag2PC) ) ) 

treebag2$fit[[1]]$fit

TestPCbag <- predict(treebag2, TestCleaned2[,-1]) # unable to make prediction values based on test cases

# Fit a model in use of Random Forests
modelRF <- train(as.numeric(classe) ~ ., data=TrainCleaned2, method="rf", prox=TRUE)
getTree( modelRF$finalModel, k=2)
predRF <- predict(modelRF, TestCleaned2[,-1])

confusionMatrix( TrainCleaned2$classe,  as.factor( rule_pml(modelRFpc) ) ) 

# ans = rep("A", 20)
# Write into answer files
pml_write_files(ans)
