###################################################
# Spatial averages and plots for ODIN data from
# Motueka
# Author: Gustavo Olivares
###################################################
#' ---
#' title: "Mapping ODIN-SD data from Motueka 2019"
#' author: "Gustavo Olivares"
#' ---

# Libraries setup
library(readr)
library(openair)
library(ggmap)
library(ggplot2)
library(sp)
library(rgdal)



##### Register google's mapping key
register_google(key = "AIzaSyACi3pNvPQTxZWx5u0nTtke598dPqdgySg")

##### Load data ####
##### Set the working directory DB ####
work_path <- path.expand("~/repositories/motueka2019-odin-analysis/mapping/")
setwd(work_path)
data_path <- path.expand("~/repositories/motueka2019-odin-analysis/data/")

# Get the list of locations
odin_locations <- read_delim(paste0(data_path,"odin_locations.txt"),
                             "\t",
                             escape_double = FALSE,
                             trim_ws = TRUE)

# Get ODIN LONG dataset
load("~/repositories/motueka2019-odin-analysis/patched.data.PM2.5.RData")
patched.data.PM2.5 <- patched.data
rm(patched.data)
# Fix date
patched.data.PM2.5$date <- as.POSIXct(patched.data.PM2.5$date,format = "%Y-%m-%d %H:%M:%S", tz='UTC')

# Generate hourly dataset for each device
devices <- unique(patched.data.PM2.5$site)
for (device in devices){
  c_data <- subset(patched.data.PM2.5, site == device)
  if (!exists(odin.1hr)){
    odin.1hr <- timeAverage(patched.data.PM2.5,avg.time = '1 hour')
  } else{
    odin.1hr <- rbind(odin.1hr,timeAverage(patched.data.PM2.5,avg.time = '1 hour'))
  }
}

# Match location
odin.1hr$X <- NA
odin.1hr$Y <- NA

for (odin in (1:length(odin_locations$ID))) {
  indx_id <- which(odin.1hr$site == paste0("SD",odin_locations$ID[odin]))
  odin.1hr$X[indx_id] <- odin_locations$X[odin]
  odin.1hr$Y[indx_id] <- odin_locations$Y[odin]
}

#Remove data without location
odin.1hr <- subset(odin.1hr,!is.na(odin.1hr$site))

# Build average map

# Some constants #####
proj4string_NZTM <- CRS('+init=epsg:2193')
proj4string_latlon <- CRS('+init=epsg:4326')
proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

curr_data <- data.frame(ODIN = unique(odin.1hr$site))
curr_data$lat <- NA
curr_data$lon <- NA
curr_data$PM2.5 <- NA

ndev <- length(curr_data$ODIN)
for (i in (1:ndev)){
  loc_id <- which(odin_locations$ID==substr(curr_data$ODIN[i],3,6))
  if (length(loc_id)>0){
    p <- project(cbind(odin_locations$X[loc_id],odin_locations$Y[loc_id]),proj = proj4string,inv = TRUE)
    curr_data$lon[i] <- p[1]
    curr_data$lat[i] <- p[2]
    curr_data$PM2.5[i] <- mean(subset(odin.1hr,site == curr_data$ODIN[i])$pm2.5,na.rm = TRUE)
  }
}

centre_lat <- mean(curr_data$lat,na.rm = TRUE)
centre_lon <- mean(curr_data$lon,na.rm = TRUE)

## Prepare the map to plot animations #####

# Get the basemap
ca <- get_googlemap(c(centre_lon,centre_lat),
                    zoom = 14,
                    scale = 2)

# Plot campaign average map
map_out <- ggmap(ca) + 
  geom_point(data=curr_data,aes(x=lon,y=lat,colour = PM2.5), size = 5) +
  scale_color_gradient(low="white", high="red",limits=c(0, max(curr_data$PM2.5,na.rm = TRUE)), name = "PM2.5")
ggsave(filename=paste0(data_path,
                       'campaign_average_Motueka.png'),
       plot=map_out,
       width=10,
       height=10,
       units = "in")

# Plot time series
plot_tseries <- ggplot(data.frame(odin.1hr),aes(x=date)) +
  geom_line(aes(y=pm2.5,colour=site)) +
  ylab('PM2.5')
ggsave(filename = paste0(data_path,
                         't_series.png'),
       plot = plot_tseries,
       width = 12,
       height = 6,
       units = 'in')

# Plot hourly maps NO INTERPOLATION
dates <- unique(odin.1hr$date)
cmax <- 120
for (now in dates) {
  print(as.POSIXct(now, origin = '1970-01-01 00:00:00'))
  data_plot <- subset(odin.1hr, date == now)
  for (i in (1:ndev)){
    loc_id <- which(odin_locations$ID==substr(curr_data$ODIN[i],3,6))
    if (length(loc_id)>0){
      cpm2.5 <- subset(data_plot,site == curr_data$ODIN[i])$pm2.5
      if (length(cpm2.5)>0){
        curr_data$PM2.5[i] <- cpm2.5
      }
    }
  }
  map_out <- ggmap(ca) + 
    geom_point(data=curr_data,aes(x=lon,y=lat,colour = PM2.5), size = 5) +
    scale_color_gradient(low="white", high="red",limits=c(0, cmax), name = "PM2.5")
  ggsave(filename=paste0(data_path,
                         'maps_nointerpolate/time_slice',
                         as.POSIXct(now, origin = '1970-01-01 00:00:00'),
                         '.png'),
         plot=map_out,
         width=10,
         height=10,
         units = "in")
}
