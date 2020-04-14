# format historical data for map -------------------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(tidyverse)

# load data
# NOAA Ecosystem Science Division data
load("Data/ESD2008_2012dataFOREC.Rdata")
esd0812 <- x[,c("DATE_", "LONGITUDE", "LATITUDE")] 

load("Data/ESD2013_2017dataFOREC.Rdata")
esd1317 <- x[,c("DATE_", "LONGITUDE", "LATITUDE")] 

# Hawaii coral disease database data
hicordis <- read.csv( "Data/hicordis.csv", head=T)
hicordis <- hicordis[,c("Date", "Longitude", "Latitude")]
  
# Guam data
guamRaw <- read.csv("Data/HEALTH_IMPACTS_DATABASE_FOR_NOAA.csv", stringsAsFactors = F, head=T)
guam.gps <- read.csv("Data/guam_sites_with_gps.csv", stringsAsFactors = F, head=T)
guamRaw <- merge(guamRaw, guam.gps, by="SITE")
guamRaw <- guamRaw[,c("DATE", "longitude", "latitude")]

# format column names
colnames(esd0812) <- c("Date", "Longitude", "Latitude")
colnames(esd1317) <- c("Date", "Longitude", "Latitude")
colnames(hicordis) <- c("Date", "Longitude", "Latitude")
colnames(guamRaw) <- c("Date", "Longitude", "Latitude")

# format dates
esd0812$Date <- as.Date(esd0812$Date, "%d-%b-%y")
esd1317$Date <- as.Date(esd1317$Date, "%d-%b-%y")
hicordis$Date <- as.Date(hicordis$Date, "%Y-%m-%d")
guamRaw$Date <- as.Date(guamRaw$Date, "%m/%d/%Y")

# number of surveys by location with dates
esd0812 <- unique(esd0812)
esd1317 <- unique(esd1317)
hicordis <- unique(hicordis)
guamRaw <- unique(guamRaw)

historical_data <- rbind(esd0812, esd1317, hicordis, guamRaw)

historical_data <- historical_data %>%
  group_by(Longitude, Latitude) %>%
  summarize(N = length(Longitude), minYr = substr(min(Date),1,4), maxYr = substr(max(Date),1,4)) %>%
  filter(!is.na(Longitude))

# add pop up data
historical_data$survey_text <- paste0("Number of surveys: ", historical_data$N,
                                      "<br> Earliest survey: ", historical_data$minYr,
                                      "<br> Latest survey: ", historical_data$maxYr)

# make all longitudes negative to on same side of Pacific Ocean in leaflet
historical_data$Longitude <- ifelse(historical_data$Longitude>0, historical_data$Longitude-360, historical_data$Longitude)

# save data
save(historical_data, file="Compiled_data/historical_surveys.RData")

# Number of surveys -----------------------------------------------
load("Compiled_data/GA_w_predictor_data.RData")
load("Compiled_data/TLS.RData")
load("Compiled_data/BBD.RData")

# format data
ga_surveys <- merged_ga %>% group_by(Region) %>% summarize(N=length(Region))
tls_surveys <- tls %>% group_by(Region) %>% summarize(N=length(Region))
bbd_surveys <- bbd %>% group_by(Region) %>% summarize(N=length(Region))
surveys <- rbind(ga_surveys, tls_surveys, bbd_surveys)
surveys$Disease_type <- c(rep("GA", nrow(ga_surveys)), rep("TLS", nrow(tls_surveys)), rep("BBD", nrow(bbd_surveys)))

surveys$Region[surveys$Region=="MHI"|surveys$Region=="NWHI"] <- "Hawaii"
surveys$Region[surveys$Region=="MARIAN"] <- "Marianas"
surveys$Region[surveys$Region=="SAMOA"] <- "Samoa"

# save data
save(surveys, file="Compiled_data/number_surveys.RData")
