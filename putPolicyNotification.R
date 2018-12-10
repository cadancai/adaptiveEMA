
#usage:
#Rscript [path to system]/scripts/updatePolicy.R [path to system] [s3 bucket name]

args <- commandArgs(trailingOnly = TRUE)
system.dir <- args[1] 
s3.bucket <- args[2]

source(file.path(system.dir,"scripts","adapEMA.R"))

for(p.dir in list.files(file.path(system.dir,"policies"),full.names = T)){
  pid <- tail(strsplit(p.dir,split="/")[[1]],n=1)
  cat("push policy for participant",pid,"...\n")
  p.files <- list.files(p.dir,full.names = T)
  if(length(p.files)!=0){
    p.files.meta <- file.info(p.files)
    p.files.meta$f.name <- rownames(p.files.meta)
    p.files.meta <- arrange(p.files.meta,f.name)
    p.files.meta <- filter(p.files.meta,endsWith(f.name,"json"))
    policy.file <- p.files.meta$f.name[1]
    system(paste("aws s3 cp",policy.file,paste0(s3.bucket,"adaptive-ema-policies/")))
    
    #rename the policy json file
    policy.file.name <- gsub("\\.json","",tail(strsplit(policy.file,split="/")[[1]],n=1))
    file.copy(file.path(p.dir,policy.file),
                file.path(p.dir,paste0(policy.file.name,"-",gsub("\\:| ","-",format(Sys.time(),"%Y-%m-%d %H:%M:%S")),".json")))
  }else{
    cat("There is no available policy to upload to S3...\n")
  }
}

for(p.dir in list.files(file.path(system.dir,"notifications"),full.names = T)){
  pid <- tail(strsplit(p.dir,split="/")[[1]],n=1)
  cat("push notification for participant",pid,"...\n")
  p.files <- list.files(p.dir,full.names = T)
  if(length(p.files)!=0){
    p.files.meta <- file.info(p.files)
    p.files.meta$f.name <- rownames(p.files.meta)
    p.files.meta <- arrange(p.files.meta,desc(mtime))
    notification.file <- p.files.meta$f.name[1]
    system(paste("aws s3 cp",notification.file,paste0(s3.bucket,"push-notifications/"))) 
  }else{
    cat("There is no notification to schedule...")
  }
}




