
#usage:
#Rscript [path to system]/scripts/updatePolicy.R [path to system] [folder name for the project] [epsilon: exploration rate]

args <- commandArgs(trailingOnly = TRUE)
system.dir <- args[1]
project.dir <- args[2]
epsilon <- args[4] #epsilon for e-greedy, e.g., 0.5

#all the participant directories inside the project folder
data.dir <- file.path(system.dir,"data",project.dir)
part.dirs <- list.files(data.dir,full.names=T)

#directory that stores preprocessed data for each participant
prepData.dir <- file.path(system.dir,"prepData")

source(file.path(system.dir,"scripts","adapEMA.R"))

for(p.dir in list.files(prepData.dir,full.names = T)){
  updatePolicy(p.dir,system.dir,epsilon)
}