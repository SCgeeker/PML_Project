# Download data

setwd("F:/Data_science_programs/PracticalMachineLearning/PML_Projects/Writeup")
trainUrl <- https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
download(trainUrl)
testUrl <- https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
download(testUrl)

# Load and clean data
pml_training <- read.csv(file="pml-training.csv")[,-1]
pml_testing <- read.csv(file="pml-testing.csv")[,-1]


# Explorary analysis
dim(pml_training)
## 19622   159
## summary(pml_training)
table(pml_training$user_name, pml_training$classe)

dim(pml_testing)
## 20 159
## summary(pml_testing)
cbind(as.character(pml_testing$user_name), pml_testing$problem_id)

