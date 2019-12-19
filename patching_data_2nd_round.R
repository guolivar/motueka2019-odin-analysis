# Second attempt at patching the data
# Libraries setup
library(lubridate)  # for working with dates
library(dplyr)      # for manipulating data
library(reshape2)
library(readr)      # Load text data
library(openair)    # Facilitate timeseries handling

# Paths
base_path <- path.expand('~/data/Motueka/ODIN/')
files_to_work <- read.delim(paste0(base_path,'datafiles.txt'),
                              header = FALSE)
names(files_to_work) <- c('filepath')
files_to_work$filepath <- paste0(base_path,files_to_work$filepath)
# Load telemetry data
tel=read.csv(path.expand('~/data/Motueka/ODIN_telemetry/all_data20180612_20190902.txt'), header=T)
# Load SD card files
sd=data.frame()

for(filepath in files_to_work$filepath){
  df <- do.call(rbind,lapply(files_to_work$filepath, function(x) read.delim(filepath,
                                                               sep=";",
                                                               header=F,
                                                               col.names=c("A",
                                                                           "PM1",
                                                                           "PM2.5",
                                                                           "PM10",
                                                                           "X",
                                                                           "Y",
                                                                           "Z",
                                                                           "B",
                                                                           "C",
                                                                           "D",
                                                                           "E",
                                                                           "TEMP",
                                                                           "RH",
                                                                           "deviceid",
                                                                           "DEVICE",
                                                                           "date",
                                                                           "time",
                                                                           "date2",
                                                                           "time2",
                                                                           "date3",
                                                                           "time3"))))
  ##take the date time required and paste together and posix. FInd the difference and make a new one
  
  df$datetime=paste(df$date, df$time, sep=" ")
  #format = 2000/01/01 01:55:04
  df$datetime = as.POSIXct(strptime(df$datetime, format = "%Y/%m/%d %H:%M:%S", "UTC"))
  df$datetime2=paste(df$date2, df$time2, sep=" ")
  df$datetime2 = as.POSIXct(strptime(df$datetime2, format = "%Y/%m/%d %H:%M:%S", "UTC"))
  df$diff=difftime(df$datetime2,df$datetime,units="secs")#Always a few seconds difference??
  df$datetimecor=as.POSIXct(ifelse(df$datetime<="2005-06-20 23:00:00 UTC" ,
                                   as.POSIXct((df$datetime+df$diff)),as.POSIXct(df$datetime)),
                            origin="1970-01-01", tz="UTC")
  
  sd=rbind(sd,df)  
}


