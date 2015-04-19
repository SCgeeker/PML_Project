# Load required
setwd("F:/Data_science_programs/PracticalMachineLearning/PML_Projects")
source("./Writeup/Loaddata.R")
source("./Writeup/pml_write_files.R")


# Explorary analysis
dim(pml_training)
## 19622   159
#  summary(pml_training)
table(pml_training$user_name, pml_training$classe)
summary(pml_training[,c(1:6,159)])

## Total 152 variables in addition to "classe", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window"


dim(pml_testing)
## 20 159
#  summary(pml_testing)
#  cbind(as.character(pml_testing$user_name), pml_testing$problem_id)
#  table(as.character(pml_testing$user_name), pml_testing$problem_id)

vind <- c( grep("roll_belt",names(pml_training)), grep("accel_belt",names(pml_training)), 
           grep("gyros_belt",names(pml_training)),grep("magnet_belt",names(pml_training)),
           grep("accel_arm",names(pml_training)), grep("magnet_arm",names(pml_training)),
           grep("accel_dumbbell",names(pml_training)), grep("gyros_dumbbell",names(pml_training)),
           grep("magnet_dumbbell",names(pml_training)),grep("gyros_forearm",names(pml_training)),
           grep("pitch_forearm",names(pml_training))
)

# Available training data set
tmp = pml_training$classe
levels(tmp) <- c(1:5)


pml_training1 <- data.frame( pml_training[pml_training$new_window=="no",vind], classe = tmp[pml_training$new_window=="no"])

# Pre-Processing in use of PCA
library(caret)
preProc17 <- preProcess(pml_training1, method = "pca", pcaComp = 17, na.rm=TRUE)

# ans = rep("A", 20)
# Write into answer files
pml_write_files(ans)
