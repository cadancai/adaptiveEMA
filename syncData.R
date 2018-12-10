
#usage:
#Rscript [path to system]/scripts/syncData.R [s3 bucket] [path to system]

#the s3 bucket for this testing project is: s3://aema-test-513f18c7-b37f-4ad2-a428-9202919e36f7
args <- commandArgs(trailingOnly = TRUE)
s3.bucket <- args[1]
system.dir <- args[2]

system(paste("aws s3 sync",s3.bucket,system.dir))