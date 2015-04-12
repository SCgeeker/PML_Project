# Download data

#setwd("F:/Data_science_programs/PracticalMachineLearning/PML_Projects/Writeup")
#trainUrl <- https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
#download(trainUrl)
#testUrl <- https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
#download(testUrl)

# Load and clean data
pml_training <- read.csv(file="./writeup/pml-training.csv")[,-1]
pml_testing <- read.csv(file="./writeup/pml-testing.csv")[,-1]


# Explorary analysis
dim(pml_training)
## 19622   159
## summary(pml_training)
table(pml_training$user_name, pml_training$classe)
summary(pml_training[,2:6])


## Total 152 variables in addition to "classe", "user_name"                "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window"

pml_training1 <- pml_training[,-c(1:6)]

dim(pml_testing)
## 20 159
## summary(pml_testing)
cbind(as.character(pml_testing$user_name), pml_testing$problem_id)
table(as.character(pml_testing$user_name), pml_testing$problem_id)

pml_testing[,c(1:6,159)]

# Available training data set
pml_training1 <- pml_training[pml_training$new_window=="no",
                              c( grep("_belt",names(pml_training)),
                                 grep("_forearm",names(pml_training)),
                                 grep("_arm",names(pml_training)),
                                 grep("_dumbbell",names(pml_training)),
                                 159)] 

