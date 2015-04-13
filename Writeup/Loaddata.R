# Download data

#setwd("F:/Data_science_programs/PracticalMachineLearning/PML_Projects/Writeup")
#trainUrl <- https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
#download(trainUrl)
#testUrl <- https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
#download(testUrl)

# Load and clean data
pml_training <- read.csv(file="./writeup/pml-training.csv")[,-1]
pml_testing <- read.csv(file="./writeup/pml-testing.csv")[,-1]


