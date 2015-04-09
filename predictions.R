# Load required
setwd("F:/Data_science_programs/PracticalMachineLearning/PML_Projects")
source("./Writeup/Loaddata.R")
source("./Submission/pml_write_files.R")



# Write into answer files
pml_write_files(ans)
