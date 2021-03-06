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

setup_twitter_oauth(consumer_key = "XSAlq3HMJyH1tQlYrgnGque8F",
                   access_token = "1144053097313886208-DIVDY1F2jqh0pNbtJFz7Nxja3It3Xt",
                   consumer_secret = "1NOYPT7iJ3tF66NK333xcNe8mOjf15WoZGXDsjuK1jVG5dvOPJ",
                   access_secret = "x1ffRfIUltZkHKnRPBUK8oiouhcnUCp7VHCgL4W5rEdYE")

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
time_avg <- '1 hour'

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

readr::write_csv(all_data,paste0(data_path,
                                 'all_data',
                                 format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                                 format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                                 ".txt"),append = FALSE)
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

updateStatus("Latest time series from Motueka - UTC time", mediaPath = paste0(data_path,
                                                                              't_series_',
                                                                              format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                                                                              format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                                                                              ".png"))

# Upload Time Series to FTP
tseries_name <- paste0('t_series_',
                       format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                       format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                       ".png")

RCurl::ftpUpload(paste0(data_path,
                        tseries_name),
                 paste0("ftp://ftp.niwa.co.nz/incoming/GustavoOlivares/odin_motueka/",
                        tseries_name))


# Compress TXT files ####
system(paste0("tar -zcvf ",
              data_path,
              'all_data',
              format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
              format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
              ".tgz ",
              data_path,
              'all_data',
              format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
              format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
              ".txt"))

system(paste0("tar -zcvf ",
              data_path,
              'all_dataAVG',
              format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
              format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
              ".tgz ",
              data_path,
              'all_dataAVG',
              format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
              format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
              ".txt"))

## Upload data ####

print("Upload data")
RCurl::ftpUpload(paste0(data_path,
                        'all_data',
                        format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                        format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                        ".tgz"),
                 paste0("ftp://ftp.niwa.co.nz/incoming/GustavoOlivares/odin_motueka/",
                        'all_data_',
                        format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                        format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                        "_campaign.tgz"))
RCurl::ftpUpload(paste0(data_path,
                        'all_dataAVG',
                        format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                        format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                        ".tgz"),
                 paste0("ftp://ftp.niwa.co.nz/incoming/GustavoOlivares/odin_motueka/",
                        'all_dataAVG_',
                        format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                        format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                        "_campaign.tgz"))



system(paste0('rm -f ',
              data_path,
              'all_dataAVG',
              format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
              format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
              ".txt"))

# remove NA on coordinates
curr_data <- subset(curr_data,!is.na(lat))
all_data.tavg <- subset(all_data.tavg,!is.na(lat))
if (location_ok){
  coordinates(curr_data) <- ~ lon + lat
  proj4string(curr_data) <- proj4string_latlon
  coordinates(all_data.tavg) <- ~ lon + lat
  proj4string(all_data.tavg) <- proj4string_latlon
  # Re-project to NZTM #####
  all_data.tavg <- spTransform(all_data.tavg,proj4string_NZTM)

  print("Starting the interpolation")

  #Setting the  prediction grid properties #####
  cellsize <- 100 #pixel size in projection units (NZTM, i.e. metres)
  min_x <- all_data.tavg@bbox[1,1] - cellsize - 1000 #minimun x coordinate 1km south
  min_y <- all_data.tavg@bbox[2,1] - cellsize - 1000 #minimun y coordinate 1km west
  max_x <- all_data.tavg@bbox[1,2] + cellsize + 1000 #mximum x coordinate 1km north
  max_y <- all_data.tavg@bbox[2,2] + cellsize + 1000 #maximum y coordinate 1km east

  x_length <- max_x - min_x #easting amplitude
  y_length <- max_y - min_y #northing amplitude

  ncol <- round(x_length/cellsize,0) #number of columns in grid
  nrow <- round(y_length/cellsize,0) #number of rows in grid

  grid <- GridTopology(cellcentre.offset=c(min_x,min_y),cellsize=c(cellsize,cellsize),cells.dim=c(ncol,nrow))

  #Convert GridTopolgy object to SpatialPixelsDataFrame object. #####
  grid <- SpatialPixelsDataFrame(grid,
                                 data=data.frame(id=1:prod(ncol,nrow)),
                                 proj4string=proj4string_NZTM)


  # Get rid of NA containing rows
  all_data.tavg <- subset(all_data.tavg,!is.na(PM2.5))
  all_dates <- sort(unique(all_data.tavg$date))
  valid_dates <- FALSE * (1:length(all_dates))
  # limits for colorscales #####
  cmin <- min(all_data.tavg$PM2.5)
  cmax <- max(all_data.tavg$PM2.5) * 0.5
  ## Interpolate and plot #####
  ndates <- length(all_dates)
  breaks <- as.numeric(quantile((1:ndates),c(0,0.5,1), type = 1))
  nbreaks <- length(breaks)
  i <- 0
  for (d_slice in (1:ndates)){
    c_data <- subset(all_data.tavg,subset = (date==all_dates[d_slice]))

    if (length(unique(c_data$serialn))<2){
      next
    }
    valid_dates[d_slice] <- TRUE

    surf.idw <- idw(PM2.5 ~ 1,newdata = grid, locations = c_data, idp = 1,na.action = na.omit)
    surf.idw$timestamp <-d_slice
    proj4string(surf.idw) <- proj4string_NZTM

    if (i==0){
      to_rast.idw <- surf.idw
      r0.idw <- rasterFromXYZ(cbind(surf.idw@coords,surf.idw$var1.pred))
      crs(r0.idw) <- '+init=epsg:2193'
      raster_cat.idw<- r0.idw
      
      i <- 1
    }
    else {
      to_rast.idw <- surf.idw
      r0.idw <- rasterFromXYZ(cbind(surf.idw@coords,surf.idw$var1.pred))
      names(r0.idw) <- as.character(all_dates[d_slice])
      crs(r0.idw) <- '+init=epsg:2193'
      raster_cat.idw<- addLayer(raster_cat.idw,r0.idw)
    }
    rtp <- rasterToPolygons(projectRaster(r0.idw,crs = "+proj=longlat +datum=WGS84"))
    points <- data.frame(spTransform(c_data,CRS('+init=epsg:4326')))
    points$label <- substr(points$serialn,1,9)

    map_out <- ggmap(ca) + geom_polygon(data = rtp,aes(x = long, y = lat, group = group,
                                            fill = rep(rtp[[1]], each = 5)),
                             size = 0,
                             alpha = 0.85) +
      scale_fill_gradient(low="white", high="red",limits=c(0, cmax), name = "PM2.5", oob=squish) +
      geom_point(data=points,aes(x=lon,y=lat),colour = "black", size = 3) +
      geom_text(data=points,aes(x=lon,y=lat,label=label), hjust=0, colour = "gray") +
      ggtitle(paste(as.character(all_dates[d_slice]+12*3600),"NZST"))
    ggsave(filename=paste0(data_path,
                           'idw/',
                           format(all_dates[d_slice]+12*3600,
                                  format = "%Y-%m-%d %H:%M"),
                           '.png'),
           plot=map_out,
           width=6,
           height=6,
           units = "in")

  }
  save('raster_cat.idw',file = paste0(data_path,'raster_cat.idw.RData'))

  print("Done with interpolating ...")

  raster_cat_idw_LL <- projectRaster(raster_cat.idw,crs = "+proj=longlat +datum=WGS84")
  save(list = c('raster_cat_idw_LL'),file = paste0(data_path,"raster_odin_LL.RData"))
}

if (location_ok){
  print("Writing NetCDF files")
  print("IDW")
  # Write NetCDF files ####
  # IDW
  lat_dim <- unique(coordinates(raster_cat_idw_LL)[,2])
  lon_dim <- unique(coordinates(raster_cat_idw_LL)[,1])
  tim_dim <- all_dates[valid_dates ==1 ]
  nc.idw <- create.nc("odin_idw.nc")
  # Dimensions specifications
  dim.def.nc(nc.idw, "time", unlim=TRUE)
  dim.def.nc(nc.idw, "latitude",length(lat_dim))
  dim.def.nc(nc.idw, "longitude",length(lon_dim))
  # Variable specifications
  var.def.nc(nc.idw,"time","NC_INT","time")
  att.put.nc(nc.idw,"time","units","NC_CHAR","seconds since 1970-01-01 00:00:0.0")
  att.put.nc(nc.idw,"time","long_name","NC_CHAR","time")

  var.def.nc(nc.idw,"latitude","NC_FLOAT","latitude")
  att.put.nc(nc.idw,"latitude","units","NC_CHAR","degrees_north")
  att.put.nc(nc.idw,"latitude","long_name","NC_CHAR","latitude")
  att.put.nc(nc.idw,"latitude","standard_name","NC_CHAR","latitude")

  var.def.nc(nc.idw,"longitude","NC_FLOAT","longitude")
  att.put.nc(nc.idw,"longitude","units","NC_CHAR","degrees_east")
  att.put.nc(nc.idw,"longitude","long_name","NC_CHAR","longitude")
  att.put.nc(nc.idw,"longitude","standard_name","NC_CHAR","longitude")

  var.def.nc(nc.idw,"pm2p5","NC_FLOAT",c("longitude","latitude","time"))
  att.put.nc(nc.idw,"pm2p5","units","NC_CHAR","ug m**-3")
  att.put.nc(nc.idw,"pm2p5","long_name","NC_CHAR","Mass concentration of PM2.5 ambient aerosol particles in air")
  att.put.nc(nc.idw,"pm2p5","standard_name","NC_CHAR","mass_concentration_of_pm2p5_ambient_aerosol_particles_in_air")
  att.put.nc(nc.idw,"pm2p5","cell_methods","NC_CHAR","time: mean (interval: 15 minutes)")
  att.put.nc(nc.idw,"pm2p5","missing_value","NC_FLOAT",-999.9)

  # Global attributes
  att.put.nc(nc.idw,"NC_GLOBAL","title","NC_CHAR","PM2.5 interpolated surface (Inverse Square Distance)")
  att.put.nc(nc.idw,"NC_GLOBAL","Conventions","NC_CHAR","CF-1.7")
  att.put.nc(nc.idw,"NC_GLOBAL","Institution","NC_CHAR","NIWA (National Institute of Water and Atmospheric Research, Auckland, New Zealand)")
  att.put.nc(nc.idw,"NC_GLOBAL","project_id","NC_CHAR","CONA - 2018")
  att.put.nc(nc.idw,"NC_GLOBAL","history","NC_CHAR",paste0(format(max(all_data.tavg$date),format = "%Y%m%d"),
                                                           " Data generated and formatted"))
  att.put.nc(nc.idw,"NC_GLOBAL","comment","NC_CHAR","Data for visualisation only")

  # Load data
  var.put.nc(nc.idw,"latitude",lat_dim)
  var.put.nc(nc.idw,"longitude",lon_dim)
  var.put.nc(nc.idw,"time",as.numeric(tim_dim))
  rast_data <- getValues(raster_cat_idw_LL)[,(1:length(tim_dim))]
  dim(rast_data) <- c(length(lon_dim),
                      length(lat_dim),
                      length(tim_dim))
  var.put.nc(nc.idw,"pm2p5",rast_data)

  # Close the file and save
  close.nc(nc.idw)

  ## Create MP4 video ####
  print("Create videos")
  system(paste0("ffmpeg -f image2 -r 6 -pattern_type glob -i '",
                data_path,
                "idw/",
                "*.png' ",
                data_path,
                "idw/",
                format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                ".mp4"))



  ## Upload to youtube ####
  print("Upload IDW to youtube")
  system(paste0("youtube-upload --title=\"Motueka ",
                format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d %H:%M"),
                " to ",
                format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d %H:%M"),
                "\" --privacy=unlisted --client-secrets=./client_secrets.json ",
                data_path,
                "idw/",
                format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
                format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
                ".mp4",
                " --playlist=\"Motueka 2019 - ODIN\""))
  
  # Upload files
  print("Upload NC files")
  RCurl::ftpUpload(paste0(data_path,"odin_idw.nc"),
                   "ftp://ftp.niwa.co.nz/incoming/GustavoOlivares/odin_motueka/odin_idw_1hr_all.nc")

}

## Remove files ####
print("Tidying up files")
system(paste0("rm -rf ",
              data_path,
              "idw/*"))
system(paste0('rm -f ',
              data_path,
              'all_data',
              format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
              format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
              ".txt"))
system(paste0('rm -f ',
              data_path,
              'all_dataAVG',
              format(min(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),"_",
              format(max(all_data.tavg$date) + 12*3600,format = "%Y%m%d"),
              ".txt"))
system('mv *.tgz data_compressed/')
system('mv t_series*.png timeseries/')

updateStatus("Arrowtown script finised OK!")
