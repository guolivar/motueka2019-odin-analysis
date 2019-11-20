###################################################
# Read ODIN offline files (TXT format)
# Author: Gustavo Olivares
###################################################
#' ---
#' title: "ODIN-SD data from Motueka 2019"
#' author: "Gustavo Olivares"
#' ---

# Clear the environment
rm(list = ls())
# Libraries setup
library(readr)
library(openair)
base_path <- path.expand('~/data/Motueka/ODIN/')
# Paths
folders_to_work <- read.delim(paste0(base_path,'dirs'),
                              header = FALSE)
#for (c_dir in as.array(folders_to_work$V1)){
  c_dir <- '00153/'
  datapath <- paste0(base_path,c_dir)
  print(datapath)
  datafile <- 'DATA.TXT'
  
  # Read data file
  if (!file.exists(paste0(datapath,datafile))){
    next
  }
  odin.data <- try(read_delim(paste0(datapath,datafile),
                              skip = 1,
                              delim = ";",
                              col_types = 'iiiiiiicniinnnccccccc',
                              col_names = c('framelength',
                                            'PM1',
                                            'PM2.5',
                                            'PM10',
                                            'PM1x',
                                            'PM2.5x',
                                            'PM10x',
                                            'GAS1sn',
                                            'GAS1',
                                            'TGAS1',
                                            'GAS2',
                                            'Temperature',
                                            'RH',
                                            'DeviceID',
                                            'Serialn',
                                            'Day',
                                            'Time',
                                            'Day2',
                                            'Time2',
                                            'Day3',
                                            'Time3'),
                              escape_double = TRUE,
                              trim_ws = TRUE))
  if (inherits(odin.data,"try-error")){
    next
  }
  
  
  
  # Correct date in data
  # Find the observations with Date2 and Date3
  odin.data$Day[is.na(odin.data$Day)] <- ""
  odin.data$Day2[is.na(odin.data$Day2)] <- ""
  odin.data$Day3[is.na(odin.data$Day3)] <- ""
  # Generate the 3 date fields
  # Clear date entries where logging failed
  nodate <- which(nchar(paste(odin.data$Day,odin.data$Time))!=19)
  odin.data$Day[nodate] <- NA
  odin.data$Time[nodate] <- NA
  idx <- !is.na(odin.data$Day)
  odin.data$date1[idx] <- as.POSIXct(paste(odin.data$Day[idx],odin.data$Time[idx]),tz='NZST')
 
  odin.data$date2 <- odin.data$date1
  nodate <- which(nchar(paste(odin.data$Day2,odin.data$Time2))!=19)
  odin.data$Day2[nodate] <- NA
  odin.data$Time2[nodate] <- NA
  odin.data$Day2[is.na(odin.data$Day2)] <- ""
  indx <- (odin.data$Day2>"2019")
  
  odin.data$date3 <- as.POSIXct(paste(odin.data$Day3,odin.data$Time3),tz='NZST')
  nodate <- which(nchar(paste(odin.data$Day3,odin.data$Time3))!=19)
  odin.data$Day3[nodate] <- NA
  odin.data$Time3[nodate] <- NA
  odin.data$Day3[is.na(odin.data$Day3)] <- ""
  indx <- (odin.data$Day3>"2019")
  
  odin.data$date3 <- odin.data$date1
  odin.data$date3[indx] <- as.POSIXct(paste(odin.data$Day3[indx],odin.data$Time3[indx]),tz='NZST')
  
  
  # Find the first record with "correct" data
  first_date <- which(odin.data$Day>"2019")[1]-1
  if (first_date>0){
    # Use this observation to correct the clock in ALL date1 clock not set records
    timediff <- odin.data$date2[first_date] - odin.data$date3[first_date]
    to_correct <- (odin.data$Day < "2019") & (odin.data$Day2 < "2019")
    odin.data$date1[to_correct] <- odin.data$date1[to_correct] + timediff
  }
  
  # Fix reboots with lost power
  to_correct <- odin.data$date1 < as.POSIXct("2019-05-01 00:00:00",tz='NZST')
  odin.data$date1[to_correct] <- odin.data$date1[to_correct] + (odin.data$date2[to_correct] - odin.data$date3[to_correct])
  
  # Use READING date for timestamp
  odin.data$date <- odin.data$date1
  # remove data without correct date
  odin.data <- subset(odin.data, date > as.POSIXct('2019-01-01 00:00:00', tz= "NZST"))
  
  # Prepare data to be merged with telemetered data
  # 10 minutes average starting on "2019-04-25 00:00:00 NZST"
  odin.data.10min <- timeAverage(odin.data,avg.time = '10 min',data.thresh = 0.75,start.date = "2019-04-25 00:00:00")
  odin.data.10min$Serialn <- odin.data$Serialn[1]
  # Remove extra columns
  odin.data.10min$framelength <- NULL
  odin.data.10min$date1 <- NULL
  odin.data.10min$date2 <- NULL
  odin.data.10min$date3 <- NULL
  odin.data.10min$PM1x <- NULL
  odin.data.10min$PM10x <- NULL
  odin.data.10min$PM2.5x <- NULL
  # Add "COARSE" data
  odin.data.10min$PMc <- odin.data.10min$PM10 - odin.data.10min$PM2.5
  
  write_csv(odin.data.10min,paste0(datapath,'data_10min.txt'))

  
  
  