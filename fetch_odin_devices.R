##### Load relevant packages #####
library(readr)
library(RJSONIO)
library(curl)
library(base64enc)

##### Set the working directory DB ####
setwd("~/repositories/motueka2019-odin-analysis/mapping/")
data_path <- "./"
##### Read the credentials file (ignored by GIT repository) ####
# Read the secrets
secret_hologram <- read_delim("./secret_hologram.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

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

write_delim(all_devices,"./all_odin_devices.csv",delim = '\t')
