# Extract SST metrics to survey data ---------------

# load libraries
library(tidyverse)
library(zoo)
library(raster)

# load surey data
load("Compiled_data/Survey_points.RData")

# SST anomalies ------------------------------------
surveyDatesRaw <- sort(unique(surveys$Date))
surveyDates <- gsub("-", "", surveyDatesRaw)

newFileName <- "Compiled_data/sst_anomalies_surveys.csv"
ssta.df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(ssta.df) <- c("Longitude", "Latitude", "Date", "SST_anomalies")
write.csv(ssta.df, newFileName, row.names = F)

for(i in 1:length(surveyDates)){
  yr <- substr(surveyDates[i], 1, 4)
  ncFileName <- paste0("E:/SSTanomaly_CRW/", yr, "/ct5km_ssta_v3.1_", surveyDates[i], ".nc")
  ncTempBrick <- brick(ncFileName)
  # do not worry about warning messages, they are providing information about the "brick" function
  surveySSTa <- extract(ncTempBrick, cbind(surveys$Longitude, surveys$Latitude))
  temp.df <- data.frame("Longitude" = surveys$Latitude, "Longitude" = surveys$Longitude, "Date" = surveyDatesRaw[i], "SST_anomalies" = surveySSTa[,])
  write.table(temp.df, file = newFileName, row.names = F, sep = ",", col.names = !file.exists(newFileName), append = T)
  cat("finished", i, "of", length(surveyDates), ":", as.character(surveyDatesRaw[i]), "\n") # uncomment if you want a print message of progress
}

# format and merge sst.df data with original survey data
sstaDF <- read_csv(newFileName)
surveys_ssta <- surveys %>% left_join(sstaDF, by = c("Date", "Latitude", "Longitude"))
# surveys_ssta <- merge(surveys, sstaDF, by = c("Date", "Latitude", "Longitude"), all.x = T)
save(surveys_ssta, file = "Compiled_data/SSTa_surveys.RData")

# SST ---------------------------------------
# dates to extract temperature data (survey dates - 90 days)
dates_long <- surveyDatesRaw

for(i in 1:length(surveyDatesRaw)){
  x <- seq.Date(surveyDatesRaw[i]-90, surveyDatesRaw[i], by = "days")
  dates_long <- c(dates_long, x)
}

dates_long <- sort(unique(dates_long))
surveyDates2 <- gsub("-", "", dates_long)

# extract SST data
newFileName2 <- "Compiled_data/sst_surveys.csv"
sst.df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(sst.df) <- c("Longitude", "Latitude", "Date", "SST")
write.csv(sst.df, newFileName2, row.names = F)

for(i in 1:length(surveyDates2)){
  yr <- substr(surveyDates2[i], 1, 4)
  ncFileName <- paste0("E:/SST_CoralReefWatch/", yr, "/coraltemp_v1.0_", surveyDates2[i], ".nc")
  ncTempBrick <- brick(ncFileName, varname = "analysed_sst")
  # do not worry about warning messages, they are providing information about the "brick" function
  surveySST <- extract(ncTempBrick, cbind(surveys$Longitude, surveys$Latitude))
  temp.df <- data.frame("Longitude" = surveys$Latitude, "Longitude" = surveys$Longitude, "Date" = dates_long[i], "SST" = surveySST[,])
  write.table(temp.df, file = newFileName2, row.names = F, sep = ",", col.names = !file.exists(newFileName2), append = T)
  cat("finished", i, "of", length(surveyDates2), ":", as.character(dates_long[i]), "\n") # uncomment if you want a print message of progress
}

surveys_sst <- read_csv("Compiled_data/sst_surveys.csv")
surveys_sst$SST_mean90d <- rollapply(surveys_sst$SST, width = 90, FUN = mean, align = "right", fill = NA, na.rm = T) 
save(surveys_sst, file = "Compiled_data/SST_surveys.RData")