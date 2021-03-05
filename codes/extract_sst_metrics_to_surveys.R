# Extract SST metrics to survey data ---------------------------------------------------------------

# load libraries
library(tidyverse)
library(zoo)
library(raster)

# load surey data
load("Compiled_data/Survey_points_with_dates.RData")

# expand dataframe to include row for all 90 days prior to survey date  ----------------------------
surveys$Survey_Date <- surveys$Date

surveys_90d <- surveys %>%
  group_by(Survey_Date, Latitude, Longitude) %>%
  complete(Date = seq.Date(Date - 90, Date, by="day"))# %>%

save(surveys_90d, file = "Compiled_data/suveys_90d_expanded.RData")

# extract SST data ---------------------------------------------------------------------------------
load("Compiled_data/suveys_90d_expanded.RData")

dates_long <- sort(unique(surveys_90d$Date))
surveyDates2 <- gsub("-", "", dates_long)

newFileName2 <- "Compiled_data/sst_surveys.csv"
sst.df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(sst.df) <- c("Latitude", "Latitude", "Date", "SST")
write.csv(sst.df, newFileName2, row.names = F)

for(i in 1:length(surveyDates2)){
  yr <- substr(surveyDates2[i], 1, 4)
  ncFileName <- paste0("I:/SST_CoralReefWatch/", yr, "/coraltemp_v1.0_", surveyDates2[i], ".nc") # on laptop it's E:/
  ncTempBrick <- brick(ncFileName, varname = "analysed_sst")
  # do not worry about warning messages, they are providing information about the "brick" function
  df <- subset(surveys_90d, Date == dates_long[i])
  surveySST <- extract(ncTempBrick, cbind(df$Longitude, df$Latitude))
  if(anyNA(surveySST) == TRUE){
    surveySST <- extract(ncTempBrick, cbind(df$Longitude, df$Latitude), method = "bilinear")
    temp.df <- data.frame("Latitude" = df$Latitude, "Longitude" = df$Longitude, "Date" = dates_long[i], "SST" = surveySST)
  } else {
    temp.df <- data.frame("Latitude" = df$Latitude, "Longitude" = df$Longitude, "Date" = dates_long[i], "SST" = surveySST[,])
  }
  write.table(temp.df, file = newFileName2, row.names = F, sep = ",", col.names = !file.exists(newFileName2), append = T)
  # indicate progress periodically
  if(i %in% seq(500, 5000, by = 500)){
    cat("finished", i, "of", length(surveyDates2), ":", as.character(dates_long[i]), "\n") # uncomment if you want a print message of progress
  }
}



# calculate 90 day mean sst for all surveys -------------------------------------------------------
# load data from above
load("Compiled_data/suveys_90d_expanded.RData")
sst <- read.csv("Compiled_data/sst_surveys.csv")

# format data
surveys_90d <- as.data.frame(surveys_90d)
sst$Date <- as.Date(sst$Date,"%Y-%m-%d")

# make SST calculations and rename survey date
sst_90d <- surveys_90d %>%
  left_join(sst, by = c("Date", "Latitude", "Longitude")) %>%
  group_by(Survey_Date, Longitude, Latitude) %>%
  summarise(SST_90dMean = mean(SST, na.rm = T)) %>%
  rename(Date = Survey_Date)

# save
save(sst_90d, file = "Compiled_data/surveys_sst_90dMean.RData")
