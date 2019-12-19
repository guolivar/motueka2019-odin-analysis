# Second attempt at patching the data
# Libraries setup
library(lubridate)  # for working with dates
library(dplyr)      # for manipulating data
library(reshape2)
library(parallel)
library(doParallel)

# Paths
base_path <- path.expand('~/repositories/motueka2019-odin-analysis/')
setwd(base_path)
files_to_work <- read.delim(paste0(base_path,'data/datafiles.txt'),
                              header = FALSE)
names(files_to_work) <- c('filepath')
files_to_work$filepath <- paste0(base_path,'data/',files_to_work$filepath)
# Load telemetry data
tel=read.csv(paste0(base_path,'data/all_data20180612_20190902.txt'), header=T)
##date & timestamp 2019-09-01T22:12:38Z 2019-09-01T22:12:55Z
tel$date = as.POSIXct(strptime(tel$date, format = "%Y-%m-%dT%H:%M:%SZ", "UTC"))
tel$timestamp = as.POSIXct(strptime(tel$timestamp, format = "%Y-%m-%dT%H:%M:%SZ", "UTC"))

# Load SD card files
sd=data.frame()

for(filepath in files_to_work$filepath){
  print(filepath)
  df <- read.delim(filepath,
                   sep=";",
                   header=FALSE,
                   fill=TRUE,
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
                               "time3"))
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

##Onwards - how to match sites bewtween sd  and tel
# use "deviceid" as the common identifier

###SO now we have matched sites and corrected dates
#Put tel & SD data together####
#for  each tel$deviceid take each tel$date and if there is PM2.5 write it, and if there is not then 
#go to sd for deviceid and find datetimecor and write that sd$PM2.5 

sites=unique(tel$deviceid)    ####note from me now: this should have pointed to tel$site, however it shouldn't be relevant to our current issue
t5mins= seq(ISOdatetime(2019,4,1,0,5,0, tz="UTC"), ISOdatetime(2019,9,15,0,0,0,tz="UTC"), by=(60*5)) 

test2=data.frame(site=numeric(),date=character(),pm2.5=numeric())

cores <- detectCores()
cl <- makeCluster(4) #not to overload your computer
registerDoParallel(cl)

nsites <- length(sites)

patched.data <- foreach(i=1:nsites,
                    .packages=c("lubridate","dplyr"),
                    .combine=rbind,
                    .errorhandling = 'remove') %dopar%
                    {
                      test2=data.frame(site=numeric(),date=character(),pm2.5=numeric())
                      dftel=subset(tel, (deviceid)==i)
                      ###do these to 5min
                      dfsd=subset(sd, (deviceid)==i)
                      ###do these to 5min
                      
                      dftel$DeviceTime5min <- floor_date(dftel$date, "5 mins")
                      dftel_5min <- dftel %>% group_by(DeviceTime5min) %>% summarize(mean(PM2.5))
                      dftel_5min<- as.data.frame(dftel_5min)
                      
                      dfsd$DeviceTime5min <- floor_date(dfsd$datetimecor, "5 mins")
                      dfsd_5min <- dfsd %>% group_by(DeviceTime5min) %>% summarize(mean(PM2.5))
                      dfsd_5min <-as.data.frame(dfsd_5min)
                      
                      for (t in t5mins){
                        t=as.POSIXct(t,origin="1970-01-01", tz="UTC")
                        pmtel=subset(dftel_5min[,2], (dftel_5min[,1]==t))
                        pmsd=subset(dfsd_5min[,2], (dfsd_5min[,1]==t))
                        
                        
                        df= data.frame(site=as.character(i),
                                       date=as.character(t),
                                       pm2.5=ifelse(length(pmtel)>0 ,pmtel,ifelse(length(pmsd)>0,pmsd, NA))  
                                       
                        )
                        test2=rbind(test2,df)        
                      }
                      test2
                    }
stopCluster(cl)

save(patched.data, file = './patched.data.RData')

