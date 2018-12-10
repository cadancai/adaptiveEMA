
#usage:
#Rscript [path to system]/scripts/copyData.R [path to system] [folder name for the project]

args <- commandArgs(trailingOnly = TRUE)
system.dir <- args[1] 
project.dir <- args[2]

#all the participant directories inside the project folder
data.dir <- file.path(system.dir,"data",project.dir)
part.dirs <- list.files(data.dir,full.names=T)

#directory that stores new data for each participant
newData.dir <- file.path(system.dir,"newData")

#directory that stores preprocessed data for each participant
prepData.dir <- file.path(system.dir,"prepData")

source(file.path(system.dir,"scripts","adapEMA.R"))

for(p.dir in list.files(newData.dir,full.names = T)){
  #preprocess sensus data for participant
  prepPart(p.dir,prepData.dir)
  
  #remove the data files in newData/pid folder
  file.remove(list.files(p.dir,full.names = T))
}



