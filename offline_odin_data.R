###################################################
# Read ODIN offline files (TXT format)
# Author: Gustavo Olivares
###################################################
#' ---
#' title: "ODIN-SD data from Motueka 2019"
#' author: "Gustavo Olivares"
#' ---

# Libraries setup
library(readr)
library(openair)

# Paths
datapath <- path.expand('~/data/Motueka/ODIN/00003/')
datafile <- 'DATA.TXT'

# Read data file
odin.data <- read.delim(paste0(datapath,datafile),
                        header = FALSE,
                        skip = 1,
                        sep = ";",
                        colClasses = c('integer',
                                       'integer',
                                       'integer',
                                       'integer',
                                       'integer',
                                       'integer',
                                       'integer',
                                       'character',
                                       'numeric',
                                       'integer',
                                       'integer',
                                       'numeric',
                                       'numeric',
                                       'numeric',
                                       'character',
                                       'character',
                                       'character',
                                       'character',
                                       'character',
                                       'character',
                                       'character'),
                        col.names = c('framelength',
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
                                      'Time3'))



# Correct date in data
timediff <- start_timestamp - as.POSIXct(paste(odin.data$Day,odin.data$Time)[1],
                                         format = '%d/%m/%Y %H:%M:%S', tz = 'PST')
odin.data$date <- as.POSIXct(paste(odin.data$Day,odin.data$Time),
                             format = '%d/%m/%Y %H:%M:%S', tz = 'PST') + timediff

# Plot timeseries
timePlot(odin.data,pollutant = c('PM1','PM2.5','PM10'),
         group = TRUE,
         avg.time = '10 min',
         y.relation = 'free',
         main = odin.data$Serialn[1])

timeVariation(odin.data,pollutant = c('PM1','PM2.5','PM10'),
              main = odin.data$Serialn[1])

timePlot(odin.data,pollutant = c('Temperature','RH'),
         group = FALSE,
         avg.time = '1 hour',
         y.relation = 'free',
         main = odin.data$Serialn[1])

timeVariation(odin.data,pollutant = c('Temperature','RH'),
              main = odin.data$Serialn[1])
