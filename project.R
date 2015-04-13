# Load required
setwd("F:/Data_science_programs/PracticalMachineLearning/PML_Projects")
source("./Writeup/Loaddata.R")
source("./Writeup/pml_write_files.R")


# Explorary analysis
dim(pml_training)
## 19622   159
## summary(pml_training)
table(pml_training$user_name, pml_training$classe)
summary(pml_training[,2:6])

## Total 152 variables in addition to "classe", "user_name"                "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window"


dim(pml_testing)
## 20 159
## summary(pml_testing)
cbind(as.character(pml_testing$user_name), pml_testing$problem_id)
table(as.character(pml_testing$user_name), pml_testing$problem_id)

# Available training data set
pml_training1 <- pml_training[pml_training$new_window=="no",-c(1:6)]


# ans = rep("A", 20)
# Write into answer files
pml_write_files(ans)
