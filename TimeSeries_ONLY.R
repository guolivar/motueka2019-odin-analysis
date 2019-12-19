##### Load relevant packages #####
library(readr)
library(reshape2)
library(automap)
library(raster)
library(gstat)
library(sp)
library(rgdal)
library(ggmap)
library(scales)
library(gstat)
library(RNetCDF)
library(RJSONIO)
library(curl)
library(base64enc)
library(zoo)
library(openair)
library(stringi)
library(viridis)
library(dplyr)
library(RColorBrewer)
library(purrr)
library(magick)
library(twitteR)
library(parallel)
library(doParallel)


##### Register google's mapping key
register_google(key = "AIzaSyACi3pNvPQTxZWx5u0nTtke598dPqdgySg")

##### Location info up to date? ####
location_ok <- TRUE

##### Set the working directory DB ####
work_path <- path.expand("~/repositories/motueka2019-odin-analysis/mapping/")
setwd(work_path)
data_path <- "./"
##### Read the credentials file (ignored by GIT repository) ####
secret_hologram <- read_delim("./secret_hologram.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

# Get the list of locations
odin_locations <- read_delim(paste0(data_path,"odin_locations.txt"),
                             "\t", escape_double = FALSE, trim_ws = TRUE)

# Get the tag list
base_url <- "https://dashboard.hologram.io/api/1/devices/tags?"
built_url <- paste0(base_url,
                    "orgid=",secret_hologram$orgid,"&",
                    "apikey=",secret_hologram$apikey)
req1 <- curl_fetch_memory(built_url)
jreq1 <- fromJSON(rawToChar(req1$content))$data$tags
ntags <- length(jreq1)
all_tags <- data.frame(id = (1:ntags),name = NA,topic = NA)

for (i in (1:ntags)){
  all_tags$id[i] <- jreq1[[i]]$id
  all_tags$name[i] <- jreq1[[i]]$name
  all_tags$topic[i] <- paste0("_TAG_",jreq1[[i]]$id,"_")
}
wanted_tags_human <- c("motueka2019")
tags <- subset(all_tags,name %in% wanted_tags_human)
wanted_tags <-paste(tags$topic,collapse = ",")
print(wanted_tags)

# Fetch the ODIN names
base_url <- "https://dashboard.hologram.io/api/1/devices?"
built_url <- paste0(base_url,
                    "limit=500&",
                    "orgid=",secret_hologram$orgid,"&",
                    "apikey=",secret_hologram$apikey)
req1 <- curl_fetch_memory(built_url)
jreq1 <- fromJSON(rawToChar(req1$content))$data
ndevices <- length(jreq1)
all_devices <- data.frame(id = (1:ndevices),name = NA)

for (i in (1:ndevices)){
  all_devices$id[i] <- jreq1[[i]]$id
  all_devices$name[i] <- jreq1[[i]]$name
}

## Get the timeseries data #####
# UTC time start ... 24 hours ago
x_now <- Sys.time()
# This is to make a run in the past
x_now <- as.POSIXct("2019-09-01 23:00:00")
print(x_now)
x_start <- x_now - 24 * 3600
x_start <- as.POSIXct("2019-04-29 00:00:00")
t_start <- floor(as.numeric(x_start))
# UTC time end ... now
t_end <- floor(as.numeric(x_now))
# Set the averaging interval
time_avg <- '1 day'

# This is for the averagin
x_start <- x_start - 25 * 3600

print("Getting data")
# Need to go device by device for query stability
for (c_deviceid in all_devices$id){
  proceed <- substr(subset(all_devices,id == c_deviceid)$name,6,9) %in% odin_locations$ID
  if (!proceed){
    next
  }
  print(subset(all_devices,id==c_deviceid)$name)
  base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"
  print("First 1000 fetch")
  built_url <- paste0(base_url,
                      "deviceid=",c_deviceid,"&",
                      "topicnames=",wanted_tags,"&",
                      "timestart=",t_start,"&",
                      "timeend=",t_end,"&",
                      "limit=1000&",
                      "orgid=",secret_hologram$orgid,"&",
                      "apikey=",secret_hologram$apikey)
  req2 <- curl_fetch_memory(built_url)
  # Deal with 500 server errors
  if (!fromJSON(rawToChar(req2$content))$success){
    print("Fetch failed")
    next
  }
  if (fromJSON(rawToChar(req2$content))$size==0){
    print("No data")
    next
  }
  jreq2_tmp <- fromJSON(rawToChar(req2$content))$data
  x_jreq2 <- jreq2_tmp
  
  base_url <- "https://dashboard.hologram.io"
  while (fromJSON(rawToChar(req2$content))$continues){
    print("Next 1000 fetch")
    built_url <- paste0(base_url,
                        fromJSON(rawToChar(req2$content))$links[3])
    req2 <- curl_fetch_memory(built_url)
    while (req2$status_code == 500){
      Sys.sleep(20)
      req2 <- curl_fetch_memory(built_url)
    }
    jreq2_tmp <- fromJSON(rawToChar(req2$content))$data
    x_jreq2 <- append(x_jreq2,fromJSON(rawToChar(req2$content))$data)
    Sys.sleep(2)
    
  }
  print(ndata <- length(x_jreq2))
  
  if (exists("jreq2")){
    jreq2 <- append(jreq2,x_jreq2)
  } else {
    jreq2 <- x_jreq2
  }
  print("Got data")
}
print(ndata <- length(jreq2))

# We'll do this in parallel because it takes A LONG time with a few 100k records
#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(2) #not to overload your computer
registerDoParallel(cl)

all_data <- foreach(i=1:ndata,
                    .packages=c("base64enc","RJSONIO"),
                    .combine=rbind,
                    .errorhandling = 'remove') %dopar%
{
  c_data <- data.frame(id = 1)
  c_data$PM1 <- NA
  c_data$PM2.5 <- NA
  c_data$PM10 <- NA
  c_data$PMc <- NA
  c_data$GAS1 <- NA
  c_data$Tgas1 <- NA
  c_data$GAS2 <- NA
  c_data$Temperature <- NA
  c_data$RH <- NA
  c_data$date <- NA
  c_data$timestamp <- NA
  c_data$deviceid <- NA
  c_data$tags <- NA
  xxx <- rawToChar(base64decode(fromJSON(jreq2[[i]]$data)$data))
  x_payload <- fromJSON(xxx)
  payload <- unlist(x_payload)
  # {"PM1":4,"PM2.5":6,"PM10":6,"GAS1":-999,"Tgas1":0,"GAS2":204,"Temperature":7.35,"RH":80.85,"recordtime":"2018/07/11;00:21:01"}
  c_data$PM1 <- as.numeric(payload[1])
  c_data$PM2.5 <- as.numeric(payload[2])
  c_data$PM10 <- as.numeric(payload[3])
  c_data$PMc <- as.numeric(payload[3]) - as.numeric(payload[2])
  c_data$GAS1 <- as.numeric(payload[4])
  c_data$Tgas1 <- as.numeric(payload[5])
  c_data$GAS2 <- as.numeric(payload[6])
  c_data$Temperature <- as.numeric(payload[7])
  c_data$RH <- as.numeric(payload[8])
  c_data$date <- as.POSIXct(as.character(payload[9]),format = "%Y/%m/%d;%H:%M:%S",tz="UTC")
  c_data$timestamp <- as.POSIXct(jreq2[[i]]$logged,format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
  c_data$deviceid <- jreq2[[i]]$deviceid
  c_data$tags <- paste((jreq2[[i]]$tags),collapse = ",")
  c_data
}

stopCluster(cl)

all_data$serialn <- NA
device_ids <- unique(all_data$deviceid)
for (i in device_ids){
  all_data$serialn[all_data$deviceid==i] <- subset(all_devices,id==i)$name
}

# Remove index
all_data$id <- NULL
print(min(all_data$timestamp))
print(max(all_data$timestamp))
names(all_data)

# Fix wrong dates
# Clock not setup ... wrong date ... replace with server logging date
wrong_dates <- which(is.na(all_data$date) | (all_data$date <= as.POSIXct("2018/01/01")) | all_data$date > as.POSIXct(Sys.time()))
tmp_error_catching <- try(all_data$date[wrong_dates] <- all_data$timestamp[wrong_dates],
                          silent = TRUE)
# Clock in device ahead of server logging time ... wrong date ... replace with server logging date
wrong_dates <- which((all_data$date - all_data$timestamp) > 0)
tmp_error_catching <- try(all_data$date[wrong_dates] <- all_data$timestamp[wrong_dates],
                          silent = TRUE)
# No timestamp and no clock ... wrong date ... catchall step, replace with NA
wrong_dates <- which(all_data$date <= as.POSIXct("2010/01/01"))
tmp_error_catching <- try(all_data$date[wrong_dates] <- NA,
                          silent = TRUE)

# Get devices locations #####
proj4string_NZTM <- CRS('+init=epsg:2193')
proj4string_latlon <- CRS('+init=epsg:4326')
proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

curr_data <- data.frame(ODIN = unique(all_data$serialn))
curr_data$lat <- NA
curr_data$lon <- NA
ndev <- length(curr_data$ODIN)
if (location_ok){
  for (i in (1:ndev)){
    loc_id <- which(odin_locations$ID==substr(curr_data$ODIN[i],6,9))
    if (length(loc_id)>0){
      p <- project(cbind(odin_locations$X[loc_id],odin_locations$Y[loc_id]),proj = proj4string,inv = TRUE)
      curr_data$lon[i] <- p[1]
      curr_data$lat[i] <- p[2]
    }
  }
  
  centre_lat <- mean(curr_data$lat,na.rm = TRUE)
  centre_lon <- mean(curr_data$lon,na.rm = TRUE)
  
  curr_data$PM1 <- NA
  curr_data$PM2.5 <- NA
  curr_data$PM10 <- NA
  curr_data$Temperature <- NA
  curr_data$RH <- NA
  ## Prepare the map to plot animations #####
  
  # Get the basemap
  ca <- get_googlemap(c(centre_lon,centre_lat),
                      zoom = 15,
                      scale = 2)
}



# Calculate averaged time series
cl <- makeCluster(2) #not to overload your computer
registerDoParallel(cl)

all_data.tavg <- foreach(i=1:length(device_ids),
                         .packages=c("openair"),
                         .combine=rbind,
                         .errorhandling = 'remove') %dopar%
  {
  device_now <- subset(all_devices,id==device_ids[i])
  some_data <- subset(all_data, serialn == device_now$name)
  avg_data <- timeAverage(some_data,
                          avg.time = time_avg,
                          start.date = strftime(x_start, format = "%Y-%m-%d %H:00:00"))
  avg_data$serialn <- subset(all_devices,id==device_ids[i])$name
  avg_data$lat <- NA
  avg_data$lon <- NA
  ### Get LAT LON from curr_data
  if (is.numeric(subset(curr_data, ODIN == device_now$name)$lat)){
    avg_data$lat <- subset(curr_data, ODIN == device_now$name)$lat
    avg_data$lon <- subset(curr_data, ODIN == device_now$name)$lon
  }
  avg_data
}
stopCluster(cl)

readr::write_csv(all_data.tavg,paste0(data_path,
                                 'all_dataAVG',
                                 format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                                 format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                                 ".txt"),append = FALSE)

plot_tseries <- ggplot(data.frame(all_data.tavg),aes(x=date)) +
  geom_line(aes(y=PM2.5,colour=serialn))
ggsave(filename = paste0(data_path,
                         't_series_',
                         format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                         format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                         ".png"),
       plot = plot_tseries,
       width = 12,
       height = 6,
       units = 'in')
# Data only from the campaign
all_data <- subset(all_data,date > as.POSIXct('2019-04-01 00:00:00'))
all_data.tavg <- subset(all_data.tavg,date > as.POSIXct('2019-04-01 00:00:00'))

# Load partisol
partisol <- read_delim("/data/data_gustavo/repositories/motueka2019-odin-analysis/mapping/partisol.txt", 
                       "\t", escape_double = FALSE, col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                       trim_ws = TRUE)

# Wide averaged data
all_data.wide <- dcast(all_data, date ~ serialn, value.var="PM10",fun.aggregate = mean)
all_data.tavg.wide <- timeAverage(all_data.wide,avg.time = '1 day')

all_data.tavg$day <- as.factor(all_data.tavg$date)

#Join Partisol
all_data.tavg <- merge(all_data.tavg,partisol,by='date',all = TRUE)
p <- ggplot(all_data.tavg, aes(x=day, y=PM10)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  geom_point(aes(x=date,y=Partisol))
p

tdc_daily.STATS <- as.data.frame(all_data.tavg.wide[,1])
tdc_daily.STATS$mean <- rowMeans(all_data.tavg.wide[,2:18],na.rm = TRUE)
tdc_daily.STATS$MIN <- apply(all_data.tavg.wide[,2:18],1,function(x) min(x,na.rm = TRUE))
tdc_daily.STATS$MAX <- apply(all_data.tavg.wide[,2:18],1,function(x) max(x,na.rm = TRUE))
readr::write_csv(tdc_daily.STATS,paste0(data_path,
                                        'data_stats_daily.txt'),append = FALSE)












tdc_hourly <- read_csv("~/repositories/motueka2019-odin-analysis/mapping/tdc_hourly.csv", 
                       col_types = "cnnnnnnnnnnnnnnnnnnnnnn")
tdc_hourly$date <- as.POSIXct(tdc_hourly$date,format = "%Y-%m-%d-%H", tz = "NZST")
timePlot(tdc_hourly,pollutant = names(tdc_hourly)[2:23], avg.time = '1 day', group = TRUE)
tdc_daily.MEAN <- timeAverage(tdc_hourly,avg.time = '1 day', statistic = 'mean')
rowMeans(tdc_daily.MEAN[,2:23],na.rm = TRUE)
tdc_daily.STATS <- tdc_daily.MEAN[,1:2]
tdc_daily.STATS$SD0003 <- NULL
tdc_daily.STATS$mean <- rowMeans(tdc_daily.MEAN[,2:23],na.rm = TRUE)
tdc_daily.STATS$MIN <- apply(tdc_daily.MEAN[,2:23],1,function(x) min(x,na.rm = TRUE))
tdc_daily.STATS$MAX <- apply(tdc_daily.MEAN[,2:23],1,function(x) max(x,na.rm = TRUE))
readr::write_csv(tdc_daily.STATS,paste0(data_path,
                                      'data_stats_daily.txt'),append = FALSE)







