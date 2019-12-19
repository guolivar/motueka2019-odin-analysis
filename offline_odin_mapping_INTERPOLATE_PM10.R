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
library(raster)
library(scales)
library(gstat)
library(RNetCDF)

# library(reshape2)
# library(automap)
# library(RJSONIO)
# library(curl)
# library(base64enc)
# library(zoo)
# library(stringi)
# library(viridis)
# library(dplyr)
# library(RColorBrewer)
# library(purrr)
# library(magick)

##### Register google's mapping key
register_google(key = "AIzaSyACi3pNvPQTxZWx5u0nTtke598dPqdgySg")

##### Load data ####
##### Set the working directory DB ####
work_path <- path.expand("~/repositories/motueka2019-odin-analysis/mapping/")
setwd(work_path)
data_path <- path.expand("./")

# Some constants #####
proj4string_NZTM <- CRS('+init=epsg:2193')
proj4string_latlon <- CRS('+init=epsg:4326')
proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

# Get the list of locations
odin_locations <- read_delim(paste0(data_path,"odin_locations.txt"),
                             "\t",
                             escape_double = FALSE,
                             trim_ws = TRUE)

# Get ODIN LONG dataset
odin.1hr <- read_delim(paste0(data_path,"data/tdc_hourly_longPM10.csv"),
           ",",
           escape_double = FALSE,
           trim_ws = TRUE,
           col_types = 'cTn')

# Match location
odin.1hr$X <- NA
odin.1hr$Y <- NA
odin.1hr$lat <- NA
odin.1hr$lon <- NA

# Correct ID to point to serial number rather than site ID
site2serial <- read_delim('./site_odin_key.txt',
                          "\t")
for (i in (1:length(site2serial$SiteID))) {
  indx_id <- which(odin.1hr$site == site2serial$SiteID[i])
  odin.1hr$site[indx_id] <- paste0('SD',site2serial$ODINSD[i])
}

for (odin in (1:length(odin_locations$ID))) {
  indx_id <- which(odin.1hr$site == paste0("SD",odin_locations$ID[odin]))
  odin.1hr$X[indx_id] <- odin_locations$X[odin]
  odin.1hr$Y[indx_id] <- odin_locations$Y[odin]
  p <- project(cbind(odin_locations$X[odin],odin_locations$Y[odin]),proj = proj4string,inv = TRUE)
  odin.1hr$lat[indx_id] <- p[2]
  odin.1hr$lon[indx_id] <- p[1]
}

#Remove data without location
odin.1hr <- subset(odin.1hr,!is.na(odin.1hr$site))
names(odin.1hr) <- c('site','date','pm','X','Y','lat','lon')
# Build average map



curr_data <- data.frame(ODIN = unique(odin.1hr$site))
curr_data$lat <- NA
curr_data$lon <- NA
curr_data$PM10 <- NA
curr_data$PM10min <- NA
curr_data$PM10max <- NA

ndev <- length(curr_data$ODIN)
for (i in (1:ndev)){
  loc_id <- which(odin_locations$ID==substr(curr_data$ODIN[i],3,6))
  if (length(loc_id)>0){
    p <- project(cbind(odin_locations$X[loc_id],odin_locations$Y[loc_id]),proj = proj4string,inv = TRUE)
    curr_data$lon[i] <- p[1]
    curr_data$lat[i] <- p[2]
    curr_data$PM10[i] <- mean(subset(odin.1hr,site == curr_data$ODIN[i])$pm,na.rm = TRUE)
    if(!is.finite(curr_data$PM10[i])){
      curr_data$PM10[i] <- NaN
    }
    curr_data$PM10min[i] <- min(subset(odin.1hr,site == curr_data$ODIN[i])$pm,na.rm = TRUE)
    if(!is.finite(curr_data$PM10min[i])){
      curr_data$PM10min[i] <- NaN
    }
    curr_data$PM10max[i] <- max(subset(odin.1hr,site == curr_data$ODIN[i])$pm,na.rm = TRUE)
    if(!is.finite(curr_data$PM10max[i])){
      curr_data$PM10max[i] <- NaN
    }
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
  geom_point(data=curr_data,
             aes(x=lon,
                 y=lat,
                 colour = PM10),
             size = 5) +
  scale_color_gradient(low="white",
                       high="red",
                       limits=c(min(curr_data$PM10,na.rm = TRUE),
                                max(curr_data$PM10,na.rm = TRUE)),
                       name = "PM10")
ggsave(filename=paste0(data_path,
                       'campaign_average_PM10_Motueka.png'),
       plot=map_out,
       width=10,
       height=10,
       units = "in")

# Plot campaign max map
map_out <- ggmap(ca) + 
  geom_point(data=curr_data,
             aes(x=lon,
                 y=lat,
                 colour = PM10max),
             size = 5) +
  scale_color_gradient(low="white", high="red",
                       limits=c(min(curr_data$PM10max,na.rm = TRUE),
                                max(curr_data$PM10max,na.rm = TRUE)),
                       name = "PM10")
ggsave(filename=paste0(data_path,
                       'HourlyAVG_maximum_PM10_Motueka.png'),
       plot=map_out,
       width=10,
       height=10,
       units = "in")

# Plot campaign min map
map_out <- ggmap(ca) + 
  geom_point(data=curr_data,
             aes(x=lon,
                 y=lat,
                 colour = PM10min),
             size = 5) +
  scale_color_gradient(low="white", high="red",
                       limits=c(min(curr_data$PM10min,na.rm = TRUE),
                                max(curr_data$PM10min,na.rm = TRUE)),
                       name = "PM10")
ggsave(filename=paste0(data_path,
                       'HourlyAVG_minimum_PM10_Motueka.png'),
       plot=map_out,
       width=10,
       height=10,
       units = "in")


# Plot time series
plot_tseries <- ggplot(data.frame(odin.1hr),
                       aes(x=date)) +
  geom_line(aes(y=pm,
                colour=site)) +
  ylab('PM10')
ggsave(filename = paste0(data_path,
                         'timeseries/t_series_PM10.png'),
       plot = plot_tseries,
       width = 12,
       height = 6,
       units = 'in')

## Plot hourly maps INTERPOLATED #####
# Remove NAs from curr_data and odin.1hr
curr_data <- subset(curr_data,!is.na(curr_data$lat))
odin.1hr <- subset(odin.1hr,!is.na(odin.1hr$lat))
# Set the coordinates for the objects
coordinates(curr_data) <- ~ lon + lat
proj4string(curr_data) <- proj4string_latlon
coordinates(odin.1hr) <- ~ lon + lat
proj4string(odin.1hr) <- proj4string_latlon
# Re-project to NZTM #####
odin.1hr <- spTransform(odin.1hr,proj4string_NZTM)

print("Starting the interpolation")

#Setting the  prediction grid properties #####
cellsize <- 50 #pixel size in projection units (NZTM, i.e. metres)
min_x <- odin.1hr@bbox[1,1] - cellsize - 500 #minimun x coordinate 500m south
min_y <- odin.1hr@bbox[2,1] - cellsize - 500 #minimun y coordinate 500m west
max_x <- odin.1hr@bbox[1,2] + cellsize + 500 #mximum x coordinate 500m north
max_y <- odin.1hr@bbox[2,2] + cellsize + 500 #maximum y coordinate 500m east

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
odin.1hr <- subset(odin.1hr,!is.na(pm))
all_dates <- sort(unique(odin.1hr$date))
valid_dates <- FALSE * (1:length(all_dates))
# limits for colorscales #####
cmin <- 0
cmax <- 120
ndates <- length(all_dates)

i <- 0
d_slice <- 3
for (d_slice in (1:ndates)){
  c_data <- subset(odin.1hr,subset = (date==all_dates[d_slice]))
  print(format(all_dates[d_slice]+12*3600,
               format = "%Y-%m-%d %H:%M"))
  
  if (length(unique(c_data$site))<2){
    next
  }
  valid_dates[d_slice] <- TRUE
  
  surf.idw <- idw(pm ~ 1,newdata = grid, locations = c_data, idp = 1,na.action = na.omit)
  surf.idw$timestamp <-d_slice
  proj4string(surf.idw) <- proj4string_NZTM
  
  if (i==0){
    to_rast.idw <- surf.idw
    r0.idw <- rasterFromXYZ(cbind(surf.idw@coords,surf.idw$var1.pred))
    crs(r0.idw) <- '+init=epsg:2193'
    raster_cat.idw<- r0.idw
    
    i <- 1
  } else {
    to_rast.idw <- surf.idw
    r0.idw <- rasterFromXYZ(cbind(surf.idw@coords,surf.idw$var1.pred))
    names(r0.idw) <- as.character(all_dates[d_slice])
    crs(r0.idw) <- '+init=epsg:2193'
    raster_cat.idw<- addLayer(raster_cat.idw,r0.idw)
  }
  rtp <- rasterToPolygons(projectRaster(r0.idw,crs = "+proj=longlat +datum=WGS84"))
  points <- data.frame(spTransform(c_data,CRS('+init=epsg:4326')))
  points$label <- substr(points$site,3,6)
  
  map_out <- ggmap(ca) + geom_polygon(data = rtp,aes(x = long, y = lat, group = group,
                                                     fill = rep(rtp[[1]], each = 5)),
                                      size = 0,
                                      alpha = 0.85) +
    scale_fill_gradient(low="white", high="red",limits=c(0, cmax), name = "PM10", oob=squish) +
#    geom_point(data=points,aes(x=lon,y=lat),colour = "black", size = 3) +
#    geom_text(data=points,aes(x=lon,y=lat,label=label), hjust=0, colour = "gray") +
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

print("Writing NetCDF files")
print("IDW")
# Write NetCDF files ####
# IDW
lat_dim <- unique(coordinates(raster_cat_idw_LL)[,2])
lon_dim <- unique(coordinates(raster_cat_idw_LL)[,1])
tim_dim <- all_dates[valid_dates ==1 ]
nc.idw <- create.nc("odin_PM10.nc")
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
att.put.nc(nc.idw,"NC_GLOBAL","history","NC_CHAR",paste0(format(max(odin.1hr$date),format = "%Y%m%d"),
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
              "idw/all_PM10.mp4"))



## Upload to youtube ####
print("Upload IDW to youtube")
system(paste0("youtube-upload --title=\"Motueka PM10\" --privacy=unlisted --client-secrets=./client_secrets.json ",
              data_path,
              "idw/all_PM10.mp4",
              " --playlist=\"Motueka 2019 - ODIN\""))

# Upload files
print("Upload NC files")
RCurl::ftpUpload(paste0(data_path,"odin_PM10.nc"),
                 "ftp://ftp.niwa.co.nz/incoming/GustavoOlivares/odin_motueka/odin_PM10_1hr_MOTUEKA.nc")

