# Load required
setwd("F:/Data_science_programs/PracticalMachineLearning/PML_Projects")
source("./Writeup/Loaddata.R")
source("./Writeup/pml_write_files.R")


# Explorary analysis and clean data
dim(pml_training)
## 19622   159
#  summary(pml_training)
table(pml_training$user_name, pml_training$classe)

## Total 152 variables in addition to "classe", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window"

#summary(pml_training[,c(1:6,159)])
#summary(pml_training[,-c(1:6,159)])

#dimnames( pml_training[,-c(1:6,159)][,!is.na( apply(pml_training[,-c(1:6,159)], 2, sum) )] )[[2]]
#summary( pml_training[,-c(1:6,159)][,!is.na( apply(pml_training[,-c(1:6,159)], 2, sum) )])

TrainCleaned <- data.frame(classe = pml_training[,159], pml_training[,-c(1:6,159)][,!is.na( apply(pml_training[,-c(1:6,159)], 2, sum) )])

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

TestCleaned <- data.frame(id = pml_testing[,159], pml_testing[,-c(1:6,159)][,!is.na( apply(pml_testing[,-c(1:6,159)], 2, sum) )])

# Available training data set
tmp = pml_training$classe
levels(tmp) <- c(1:5)


#pml_training1 <- data.frame( pml_training[pml_training$new_window=="no",vind], classe = tmp[pml_training$new_window=="no"])

# Pre-Processing in use of PCA
library(caret)
preProc17 <- preProcess(TrainCleaned[,-1], method = "pca", pcaComp = 17)

# Find a fitted model
TrainPC17 <- predict(preProc17,TrainCleaned[,-1])

modelFit17 <- train(as.numeric(TrainCleaned$classe) ~ ., method="glm",data=TrainPC17)

# List predictions of this fitted model
modelOUT17 <- predict(modelFit17, TrainPC17)

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

# ans = rep("A", 20)
# Write into answer files
pml_write_files(ans)
