load("~/repositories/motueka2019-odin-analysis/patched.data.PM2.5.RData")
patched.data.PM2.5 <- patched.data
rm(patched.data)
load("~/repositories/motueka2019-odin-analysis/patched.data.PM10.RData")
patched.data.PM10 <- patched.data
#Clear negative values
fixID <- which(patched.data.PM10$pm10<0)
patched.data.PM10$pm10[fixID] <- NA
rm(patched.data)
all.patched <- merge(patched.data.PM2.5,patched.data.PM10,by=c('date','site'), all = TRUE)
names(all.patched) <- c('Fdate','deviceid','PM2.5','PM10')
all.patched$date <- as.POSIXct(all.patched$Fdate, format = "%Y-%m-%d %H:%M:%S")



sites=unique(all.patched$deviceid)
j <- 1
for (i in sites){
  data2plot <- subset(all.patched, deviceid == i)
  ggscat[[j]] <- ggplot(data = data2plot) +
    geom_point(aes(x=pm10,y=pm2.5)) +
    geom_abline(slope = 1, intercept = 0) +
    ggtitle(i)
  print(ggscat)
}


for (i in sites){
  data2plot <- subset(all.patched, (deviceid == i)&!is.na(date))
  timePlot(data2plot,pollutant = c('PM2.5','PM10'), group = TRUE)
}


ggbox2.5 <- ggplot(data = all.patched) +
  geom_boxplot(aes(x=deviceid, y=pm2.5))
ggbox2.5

ggbox10 <- ggplot(data = all.patched) +
  geom_boxplot(aes(x=deviceid, y=pm10))
ggbox10

names(patched.data.PM10) <- c('site','date','value','PM')
names(patched.data.PM2.5) <- c('site','date','value','PM')

patched.data.PM10$PM <- "PM10"
patched.data.PM2.5$PM <- "PM2.5"
all.patched.long <- rbind(patched.data.PM10,patched.data.PM2.5)

ggbox <- ggplot(data = all.patched.long) +
  geom_boxplot(aes(x=site, y=value,fill = PM)) +
  ylim(c(0,500))
ggbox
