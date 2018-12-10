
if (!require(dplyr)) install.packages('dplyr')
if (!require(data.table)) install.packages('data.table')
if (!require(jsonlite)) install.packages('jsonlite')
if (!require(lubridate)) install.packages('lubridate')
library(dplyr,quietly = TRUE, warn.conflicts = FALSE)
library(data.table,quietly = TRUE, warn.conflicts = FALSE)
library(jsonlite,quietly = TRUE, warn.conflicts = FALSE)
library(lubridate,quietly = TRUE, warn.conflicts = FALSE)
library(MASS,quietly = TRUE, warn.conflicts = FALSE)


prcDt <- function(files){
  #create a list to hold the data
  dt <- list() #vector("list",length(fpaths))
  i <- 1
  for(f in files){
    cat("parsing file:",f,"\n")
    if(file.size(f)!=0){
      #parse json
      datum <- try(read_json(f,simplifyVector = T, flatten=T))
      if("try-error" %in% class(datum)){
        #if error, read in the file line by line
        con <- file(f,"r")
        line <- readLines(con)
        close(con)
        #modify the end of the file and remove the extra comma in the second to the last line
        line[length(line)] <- "]"
        substr(line[(length(line)-1)],nchar(line[(length(line)-1)]),nchar(line[(length(line)-1)])) <- " "
        tmp.file <- file.path(getwd(),"newjson.json.gz")
        #write it out to a temporary file and reprocess it
        newCon <- file(tmp.file,"w")
        writeLines(line,newCon)
        close(newCon)
        datum <- try(read_json(tmp.file,simplifyVector = T, flatten=T))
        if("try-error" %in% class(datum)) next
      }
      dt[[i]] <- datum
      i <- i + 1
    }else{
      cat(f,"is of size zero and is ignored!\n")
    }
  }
  dt <- rbindlist(dt,fill=T,use.names = T)
  return(dt)
}


copyFiles <- function(p.dir,newData.dir,ndays=1){
  pid <- tail(strsplit(p.dir,split="/")[[1]],n=1)
  cat("copying new data files for participant...\n")
  
  #identify all new files on today
  p.files <- list.files(p.dir,full.names = T)
  p.files.meta <- file.info(p.files)
  p.files.meta$f.name <- rownames(p.files.meta)
  current.day <- Sys.time() - 3600*24*(ndays-1)
  cat("current.day: ",current.day,"\n")
  prev.day <- current.day - 3600*24*ndays
  cat("previous.day: ",prev.day,"\n")
  new.files <- filter(p.files.meta,mtime>=prev.day & mtime<=current.day)
  
  #check to see if the daily data folder exists, 
  #if not create the folder, then make a copy of the new files in the new folder
  if(!file.exists(file.path(newData.dir,pid))){
    dir.create(file.path(newData.dir,pid))
  }
  file.copy(new.files$f.name, file.path(newData.dir,pid))
}

#dt is the raw sensus data
#datum is the type of datum, e.g., AccelerometerDatum
#prep.dir is the location to save the preprocessed sensus data
prepDatum <- function(dt,datum,pid,prepData.dir){
  #cat("process",datum,"\n")
  #keep only the data corresponding to datum
  datum.dt <- filter(dt,grepl(datum,type))
  datum.dt$OS <- trimws(sapply(strsplit(datum.dt$type,","),"[[",2))
  datum.dt$type <- datum
  
  #preprocess the data according to the datum
  if(datum=="AccelerometerDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,X,Y,Z)
    datum.dt <- datum.dt %>% mutate(Timestamp.posixct=as.POSIXct(
      lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")
    ))
    datum.dt$Timestamp.unix <- as.numeric(datum.dt$Timestamp.posixct)
  }
  
  if(datum=="BatteryDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,Level)
    datum.dt <- datum.dt %>% mutate(Timestamp.posixct=as.POSIXct(
      lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")
    ))
    datum.dt$Timestamp.unix <- as.numeric(datum.dt$Timestamp.posixct)
  }
  
  if(datum=="AltitudeDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,Altitude)
    datum.dt <- datum.dt %>% mutate(Timestamp.posixct=as.POSIXct(
      lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")
    ))
    datum.dt$Timestamp.unix <- as.numeric(datum.dt$Timestamp.posixct)
  }
  
  if(datum=="CompassDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,Heading)
    datum.dt <- datum.dt %>% mutate(Timestamp.posixct=as.POSIXct(
      lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")
    ))
    datum.dt$Timestamp.unix <- as.numeric(datum.dt$Timestamp.posixct)
  }
  
  if(datum=="LocationDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,Latitude,Longitude,Accuracy)
    datum.dt <- datum.dt %>% mutate(Timestamp.posixct=as.POSIXct(
      lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")
    ))
    datum.dt$Timestamp.unix <- as.numeric(datum.dt$Timestamp.posixct)
  }
  
  if(datum=="ActivityDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,Activity,Phase,State,Confidence)
    datum.dt <- datum.dt %>% mutate(Timestamp.posixct=as.POSIXct(
      lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")
    ))
    datum.dt$Timestamp.unix <- as.numeric(datum.dt$Timestamp.posixct)
  }
  
  if(datum=="WlanDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,AccessPointBSSID)
    datum.dt <- datum.dt %>% mutate(Timestamp.posixct=as.POSIXct(
      lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")
    ))
    datum.dt$Timestamp.unix <- as.numeric(datum.dt$Timestamp.posixct)
  }
  
  if(datum=="ParticipationReportDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,
                       "ProbeParticipation.$type",
                       ProbeParticipation.Sensus.iOS.Network.Probes.iOSPollingWlanProbe,
                       ProbeParticipation.Sensus.iOS.Probes.Context.iOSBluetoothDeviceProximityProbe,
                       ProbeParticipation.Sensus.iOS.Probes.Location.iOSAltitudeProbe,
                       ProbeParticipation.Sensus.iOS.Probes.Location.iOSCompassProbe,
                       ProbeParticipation.Sensus.iOS.Probes.Movement.iOSAccelerometerProbe,
                       ProbeParticipation.Sensus.iOS.Probes.Movement.iOSActivityProbe,
                       ProbeParticipation.Sensus.Probes.Device.BatteryProbe,
                       ProbeParticipation.Sensus.Probes.Location.PollingLocationProbe,
                       ProbeParticipation.Sensus.Probes.Movement.PollingSpeedProbe,
                       ProbeParticipation.Sensus.Probes.User.Scripts.ScriptProbe)
    datum.dt <- datum.dt %>% mutate(Timestamp.posixct=as.POSIXct(
      lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")
    ))
    datum.dt$Timestamp.unix <- as.numeric(datum.dt$Timestamp.posixct)
  }
  
  if(datum=="SpeedDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,KPH)
    datum.dt <- datum.dt %>% mutate(Timestamp.posixct=as.POSIXct(
      lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")
    ))
    datum.dt$Timestamp.unix <- as.numeric(datum.dt$Timestamp.posixct)
  }
  
  if(datum=="ScriptRunDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,ScriptId,ScriptName,RunId,
                       ScheduledTimestamp,Latitude,Longitude,LocationTimestamp)
    datum.dt <- datum.dt %>% mutate(
      Timestamp.posixct=as.POSIXct(lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")),
      ScheduledTimestamp.posixct=as.POSIXct(lubridate::with_tz(strptime(ScheduledTimestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")),
      LocationTimestamp.posixct=as.POSIXct(lubridate::with_tz(strptime(LocationTimestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")))
    datum.dt <- datum.dt %>% mutate(
      Timestamp.unix=as.numeric(Timestamp.posixct),
      ScheduledTimestamp.unix=as.numeric(ScheduledTimestamp.posixct),
      LocationTimestamp.unix=as.numeric(LocationTimestamp.posixct))
  }
  
  if(datum=="ScriptDatum"){
    datum.dt <- dplyr::select(datum.dt,ProtocolId,ParticipantId,DeviceId,type,OS,Timestamp,ScriptId,ScriptName,RunId,GroupId,
                       InputId,Latitude,Longitude,LocationTimestamp,RunTimestamp,SubmissionTimestamp)
    datum.dt <- datum.dt %>% mutate(
      Timestamp.posixct=as.POSIXct(lubridate::with_tz(strptime(Timestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")),
      LocationTimestamp.posixct=as.POSIXct(lubridate::with_tz(strptime(LocationTimestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")),
      RunTimestamp.posixct=as.POSIXct(lubridate::with_tz(strptime(RunTimestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")),
      SubmissionTimestamp.posixct=as.POSIXct(lubridate::with_tz(strptime(SubmissionTimestamp,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),tzone="EST")))
    datum.dt <- datum.dt %>% mutate(
      Timestamp.unix=as.numeric(Timestamp.posixct),
      LocationTimestamp.unix=as.numeric(LocationTimestamp.posixct),
      RunTimestamp.unix=as.numeric(RunTimestamp.posixct),
      SubmissionTimestamp.unix=as.numeric(SubmissionTimestamp.posixct))
  }
  write.csv(datum.dt,file=file.path(prepData.dir,pid,paste0(datum,pid,".csv")),row.names = F)
}


#preprocess the data for each participant
prepPart <- function(p.dir,prepData.dir){
  #extract the pid
  pid <- tail(strsplit(p.dir,split="/")[[1]],n=1)
  cat("preprocess data from participant...",pid,"\n")
  
  #list all the files in the participant new data folder
  p.files <- list.files(p.dir,full.names = T)
  
  #read in the raw data
  cat("read in the raw data...\n")
  dt <- as.data.frame(prcDt(p.files))
  
  #sort and remove duplicates
  cat("sort and remove duplicates...\n")
  dt <- dplyr::arrange(dt,Timestamp)
  dt <- dt[!duplicated(dt),] 
  
  #rename the first column "$type"
  names(dt)[1] <- "type"
  
  #extract the types of datum/sensor that has data in the current dataset
  sensor.types <- unique(dt$type)
  sensor.types <- sapply(strsplit(sensor.types,"\\."),function(x,pattern){
    return(x[grepl(pattern,x)])
  },pattern="Datum")
  sensor.types <- sapply(strsplit(sensor.types,","),function(x){
    return(x[1])
  })
  
  #create the participant folder if it does not exist
  if(!file.exists(file.path(prepData.dir,pid))){
    dir.create(file.path(prepData.dir,pid))
  }
  #loop through each unique datum and create the 
  #preprocessed csv data file in the prep.dir
  for(datum in sensor.types){
    cat("processing...",datum,"\n")
    prepDatum(dt,datum,pid,prepData.dir)
  }
}


initiateQ <- function(epsilon){
  Qvalue <- list()
  #assign the initial epsilon value
  Qvalue$epsilon <- epsilon
  #define features and initiate them to be zero
  Qvalue$time.feature <- list()
  Qvalue$time.feature$num.responses <- rep(0,24)
  names(Qvalue$time.feature$num.responses) <- paste0("window",seq(9.5,21,0.5))
  Qvalue$time.feature$num.prompts <- rep(0,24)
  names(Qvalue$time.feature$num.prompts) <- paste0("window",seq(9.5,21,0.5))
  return(Qvalue)
}

updateQ <- function(ema.dt,Qvalue){
  #process the ema.dt to extract state, and reward for each sample
  ema.dt <- ema.dt %>% mutate(
    reward=ifelse(!is.na(SubmissionTimestamp.posixct),1,0),
    # reward.5mins=ifelse(!is.na(SubmissionTimestamp.posixct) & 
    #                       SubmissionTimestamp.posixct - Timestamp.posixct<=300,1,0),
    time.feature=paste0("window",ifelse(as.numeric(format(Timestamp.posixct,"%M"))<30,
                                        paste0(format(Timestamp.posixct,"%H"),".5"),
                                        as.numeric(format(Timestamp.posixct,"%H"))+1)))
  
  #update the Qvalue based on the response feedback (reward)
  for(i in 1:nrow(ema.dt)){
    cat("i:",i,"\n")
    Qvalue$time.feature$num.prompts[which(names(Qvalue$time.feature$num.prompts)==ema.dt$time.feature[i])] <- 
      Qvalue$time.feature$num.prompts[which(names(Qvalue$time.feature$num.prompts)==ema.dt$time.feature[i])]  + 1
    if(ema.dt$reward[i]==1){
      Qvalue$time.feature$num.responses[which(names(Qvalue$time.feature$num.responses)==ema.dt$time.feature[i])] <- 
        Qvalue$time.feature$num.responses[which(names(Qvalue$time.feature$num.responses)==ema.dt$time.feature[i])]  + 1
    }
  }
  return(Qvalue)
}

convertQtoJson <- function(Qvalue){
  Qvalue_json <- list()
  Qvalue_json$epsilon <- Qvalue$epsilon
  Qvalue_json$time.feature <- ifelse(Qvalue$time.feature$num.responses==0,0,
                                     Qvalue$time.feature$num.responses/Qvalue$time.feature$num.prompts)
  Qvalue_json$time.feature <- lapply(Qvalue_json$time.feature,function(x){x})
  Qvalue_json <- toJSON(Qvalue_json,pretty=T,auto_unbox =T)
  return(Qvalue_json)
}


createNotification <- function(ema.dt){
  os_ <- unique(ema.dt$OS)
  notification <- list(
    id=system("uuidgen",intern = TRUE),
    device=unique(ema.dt$DeviceId),
    protocol=unique(ema.dt$ProtocolId),
    title="",
    body="",
    sound="",
    command="UPDATE-EMA-POLICY",
    format=ifelse(os_=="SensusiOS","apple","gcm"),
    "creation-time"=as.numeric(Sys.time()),
    time=0
  )
  id <- notification$id
  notification <- toJSON(notification,pretty =T,auto_unbox =T)
  return(list(notification,id))
}



updatePolicy <- function(p.dir,system.dir,epsilon){
  #create the training experiences
  pid <- tail(strsplit(p.dir,split="/")[[1]],n=1)
  if(file.exists(file.path(p.dir,paste0("ScriptRunDatum",pid,".csv")))){
    script.run.dt <- read.csv(file.path(p.dir,paste0("ScriptRunDatum",pid,".csv")),stringsAsFactors = F)
    script.run.dt.sum <- dplyr::select(script.run.dt,OS,ProtocolId,ParticipantId,DeviceId,ScriptId,RunId,Timestamp.posixct,
                                       ScheduledTimestamp.posixct,Latitude,Longitude,LocationTimestamp.posixct)
    if(file.exists(file.path(p.dir,paste0("ScriptDatum",pid,".csv")))){
      script.dt <- read.csv(file.path(p.dir,paste0("ScriptDatum",pid,".csv")),stringsAsFactors = F)
      
      #merge the script and script run data
      script.dt.sum <- as.data.frame(
        script.dt %>% group_by(ProtocolId,ParticipantId,DeviceId,ScriptId,RunId) %>% 
          summarise(RunTimestamp.posixct=first(RunTimestamp.posixct),
                    SubmissionTimestamp.posixct=first(SubmissionTimestamp.posixct),
                    StartTimestamp.posixct=min(Timestamp.posixct,na.rm = T),
                    EndTimestamp.posixct=max(Timestamp.posixct,na.rm=T),
                    Latitude=first(Latitude),Longitude=first(Longitude),
                    LocationTimestamp.posixct=first(LocationTimestamp.posixct)))
      ema.response.dt <- left_join(script.run.dt.sum,script.dt.sum,by=c("ProtocolId","ParticipantId","DeviceId","ScriptId","RunId"),
                                   suffix=c(".prompt",".response"))
      
      #convert all the time variables from char to posixct
      ema.response.dt <- ema.response.dt %>% mutate(
        Timestamp.posixct = as.POSIXct(strptime(Timestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        ScheduledTimestamp.posixct = as.POSIXct(strptime(ScheduledTimestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        LocationTimestamp.posixct.prompt = as.POSIXct(strptime(LocationTimestamp.posixct.prompt,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        RunTimestamp.posixct = as.POSIXct(strptime(RunTimestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        SubmissionTimestamp.posixct = as.POSIXct(strptime(SubmissionTimestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        StartTimestamp.posixct = as.POSIXct(strptime(StartTimestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        EndTimestamp.posixct = as.POSIXct(strptime(EndTimestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        LocationTimestamp.posixct.response = as.POSIXct(strptime(LocationTimestamp.posixct.response,format="%Y-%m-%d %H:%M:%OS",tz="EST")))
      
      #get the device ID
      deviceId <- unique(ema.response.dt$DeviceId)
      
      #rename all data files from the prepData/pid folder
      file.rename(file.path(p.dir,paste0("ScriptDatum",pid,".csv")),
                  file.path(p.dir,paste0("ScriptDatum",pid,"-",gsub("\\:| ","-",format(Sys.time(),"%Y-%m-%d %H:%M:%S")),".csv")))
      
    }else{
      cat("there is no responded EMAs during this period...")
      ema.response.dt <- script.run.dt.sum
      ema.response.dt$SubmissionTimestamp.posixct <- NA
      ema.response.dt$RunTimestamp.posixct <- NA
      ema.response.dt$StartTimestamp.posixct <- NA
      ema.response.dt$EndTimestamp.posixct <- NA
      names(ema.response.dt)[which(names(ema.response.dt) %in% c("Longitude","Latitude","LocationTimestamp.posixct"))] <-
        paste0(names(ema.response.dt)[which(names(ema.response.dt) %in% c("Longitude","Latitude","LocationTimestamp.posixct"))],".prompt")
      ema.response.dt$Latitude.response <- NA
      ema.response.dt$Longitude.response <- NA
      ema.response.dt$LocationTimestamp.posixct.response <- NA
      
      ema.response.dt <- ema.response.dt %>% mutate(
        Timestamp.posixct = as.POSIXct(strptime(Timestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        ScheduledTimestamp.posixct = as.POSIXct(strptime(ScheduledTimestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        LocationTimestamp.posixct.prompt = as.POSIXct(strptime(LocationTimestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        RunTimestamp.posixct = as.POSIXct(strptime(RunTimestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")),
        SubmissionTimestamp.posixct = as.POSIXct(strptime(SubmissionTimestamp.posixct,format="%Y-%m-%d %H:%M:%OS",tz="EST")))
      
      deviceId <- unique(script.run.dt$DeviceId)
    }
    
    # #extract state features using other Sensus datum
    # ema.response.dt <- extractState(ema.response.dt,p.dir,system.dir)
    
    #initiate Q if Q does not exist, otherwise import it
    if(!file.exists(file.path(system.dir,"policies",pid))){
      dir.create(file.path(system.dir,"policies",pid))
      Qvalue <- initiateQ(epsilon)
    }else{
      load(file.path(system.dir,"policies",pid,paste0("emaPolicy",pid,".RData"))) #load the Qvalue
      #keep a copy of the original policy as a backup for future analysis
      file.rename(file.path(system.dir,"policies",pid,paste0("emaPolicy",pid,".RData")),
                  file.path(system.dir,"policies",pid,
                            paste0("emaPolicy",pid,"-",gsub("\\:| |_","-",format(Sys.time(),"%Y-%m-%d %H:%M:%S")),".RData")))
    }
    
    #check the state, action, and the reward; and update Q
    Qvalue <- updateQ(ema.response.dt,Qvalue)
    
    #save Q as both an R object and a json file
    save(Qvalue,file=file.path(system.dir,"policies",pid,paste0("emaPolicy",pid,".RData")))
    #convert the Qvalue to its json version
    Qvalue_json <- convertQtoJson(Qvalue)
    write(Qvalue_json, file=file.path(system.dir,"policies",pid,paste0(deviceId,".json")))
    
    #create the push notification json file
    if(!file.exists(file.path(system.dir,"notifications",pid))){
      dir.create(file.path(system.dir,"notifications",pid))
    }
    notification <- createNotification(ema.response.dt)
    write(notification[[1]],file.path(system.dir,"notifications",pid,paste0(notification[[2]],".json")))
    
    #rename all data files from the prepData/pid folder
    file.rename(file.path(p.dir,paste0("ScriptRunDatum",pid,".csv")),
                file.path(p.dir,paste0("ScriptRunDatum",pid,"-",gsub("\\:| ","-",format(Sys.time(),"%Y-%m-%d %H:%M:%S")),".csv")))
    
  }else{
    cat("there is no scheduled EMAs during this period...")
  }
}

