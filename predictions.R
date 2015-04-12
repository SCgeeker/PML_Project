# Load required
setwd("F:/Data_science_programs/PracticalMachineLearning/PML_Projects")
source("./Writeup/Explore_data.R")
source("./Writeup/pml_write_files.R")


# ans = rep("A", 20)
# Write into answer files
pml_write_files(ans)
