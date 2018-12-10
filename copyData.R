
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

#copy files to the new directory for each participant
for(p.dir in part.dirs){
  copyFiles(p.dir,newData.dir,ndays=1)
}
