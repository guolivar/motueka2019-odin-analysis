library(lubridate)  # for working with dates
library(dplyr)      # for manipulating data
library(reshape2)

setwd("C:\\Users\\somerveller\\Desktop\\TDC") 
##Read in telemetered data - only need this once####
tel=read.csv("all_data20180612_20190902.txt", header=T)
##date & timestamp 2019-09-01T22:12:38Z 2019-09-01T22:12:55Z
tel$date = as.POSIXct(strptime(tel$date, format = "%Y-%m-%dT%H:%M:%SZ", "UTC"))
tel$timestamp = as.POSIXct(strptime(tel$timestamp, format = "%Y-%m-%dT%H:%M:%SZ", "UTC"))

###Look at what sites             
              
              > table(tel$serialn)
              
              ODIN-0003 (90127) ODIN-0035 (89996) ODIN-0037 (90051) ODIN-0064 (90010) ODIN-0070 (89756) 
              23579             40737             13962             36980                 2 
              ODIN-0149 (90093)  ODIN-0150(89582) ODIN-0151 (90523) ODIN-0153 (89566) ODIN-0154 (89871) 
              62522              1848             61334             39661              2991 
              ODIN-0157 (89558) ODIN-0158 (89608) ODIN-0159 (89590) ODIN-0161 (90473) ODIN-0163 (90515) 
              61541             61450             58046             31961             24353 
              ODIN-0164 (89624) ODIN-0165 (90440) ODIN-0166 (89863) ODIN-0169 (90531) 
              61207              5365             61527             34155 
              > 
                
###Read in batch of SD card data in one long file
setwd("C:\\Users\\somerveller\\Desktop\\TDC\\SDcards2")
sd=data.frame()

for(i in filenames){
  filepath <- file.path("C:\\Users\\somerveller\\Desktop\\TDC\\SDcards2",paste(i,sep=""))
    df <- do.call(rbind,lapply(filenames, function(x) read.delim(filepath, sep=";",header=F,
          col.names=c("A","PM10","PM2.5","PM1","X","Y","Z","B","C","D","E","TEMP","RH","F",
                      "DEVICE","date", "time","date2","time2", "date3","time3"))))
###THIS IS A MISNAME - IT SHOULD BE PM1, PM2.5, PM10
    ##If you ever redo, change and start with the right column headers
   sd=rbind(sd,df)  
}
rm(df)
##take the date time required and paste together and posix. FInd the difference and make a new one

sd$datetime=paste(sd$date, sd$time, sep=" ")
#format = 2000/01/01 01:55:04
sd$datetime = as.POSIXct(strptime(sd$datetime, format = "%Y/%m/%d %H:%M:%S", "UTC"))
sd$datetime2=paste(sd$date2, sd$time2, sep=" ")
sd$datetime2 = as.POSIXct(strptime(sd$datetime2, format = "%Y/%m/%d %H:%M:%S", "UTC"))
sd$diff=difftime(sd$datetime2,sd$datetime,units="secs")#Always a few seconds difference??
sd$datetimecor=as.POSIXct(ifelse(sd$datetime<="2005-06-20 23:00:00 UTC" ,
                                 as.POSIXct((sd$datetime+sd$diff)),as.POSIXct(sd$datetime)),
                          origin="1970-01-01", tz="UTC")


##Onwards - how to match sites bewtween sd$DEVICE - SD0003  and tel$serialn - ODIN-0003 (90127)
#make new tel column that matches sd naming
tel$site=ifelse(tel$serialn=="ODIN-0003 (90127)", "SD0003",
            ifelse(tel$serialn=="ODIN-0035 (89996)", "SD0035",
            ifelse(tel$serialn=="ODIN-0037 (90051)","SD0037",
            ifelse(tel$serialn=="ODIN-0064 (90010)","SD0064",
            ifelse(tel$serialn=="ODIN-0070 (89756)", "SD0070",
            ifelse(tel$serialn=="ODIN-0149 (90093)","SD0149",
            ifelse(tel$serialn=="ODIN-0150(89582)","SD0150",
           ifelse(tel$serialn=="ODIN-0151 (90523)","SD0151",
            ifelse(tel$serialn=="ODIN-0153 (89566)","SD0153",
             ifelse(tel$serialn=="ODIN-0154 (89871)","SD0154",
              ifelse(tel$serialn=="ODIN-0157 (89558)","SD0157",
               ifelse(tel$serialn=="ODIN-0158 (89608)","SD0158",
               ifelse(tel$serialn=="ODIN-0159 (89590)","SD0159",
              ifelse(tel$serialn=="ODIN-0161 (90473)","SD0161",
              ifelse(tel$serialn=="ODIN-0163 (90515)","SD0163",
             ifelse(tel$serialn=="ODIN-0164 (89624)","SD0164",
             ifelse(tel$serialn=="ODIN-0165 (90440)","SD0165",
              ifelse(tel$serialn=="ODIN-0166 (89863)","SD0166",
             ifelse(tel$serialn=="ODIN-0169 (90531)","SD0169",NA)))))))))))))))))))
           
###SO now we have matched sites and corrected dates
#Put tel & SD data together####
#for  each tel$site take each tel$date and if there is PM2.5 write it, and if there is not then 
#go to sd for DEVICE and find datetimecor and write that sd$PM2.5 

sites=unique(sd$DEVICE)    ####note from me now: this should have pointed to tel$site, however it shouldn't be relevant to our current issue
t5mins= seq(ISOdatetime(2019,4,1,0,5,0, tz="UTC"), ISOdatetime(2019,9,15,0,0,0,tz="UTC"), by=(60*5)) 

test2=data.frame(site=numeric(),date=character(),pm2.5=numeric())

for (i in sites){

dftel=subset(tel, (site)==i)
###do these to 5min
dfsd=subset(sd, (DEVICE)==i)
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
}}
rm(dfsd, dfsd_5min, dftel, dftel_5min,pm2.5,df,t,i, pmsd,pmtel)
