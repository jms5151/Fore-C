
# Extract wave energy data ---------------------------------------------------------------
# load ga data
load("Compiled_data/Survey_points.RData")

# split into 10000 row chunks to get wave energy data from sesync
surveys$long <- surveys$Longitude
surveys$lat <- surveys$Latitude
surveys_split <- split(surveys, rep(1:ceiling(nrow(surveys)/10000), each=10000, length.out=nrow(surveys)))

for(j in 1:length(surveys_split)){
  df <- surveys_split[[j]]
  write.csv(df, paste0("Data/wave_energy_surveys/surveys_", j, ".csv"), row.names = F)
}

# get data manually through https://shiny.sesync.org/apps/msec/

# Load, merge, and save wave energy data -------------------------------------------------
msec_files <- list.files("Compiled_data/msec_out")
wave_data <- data.frame() 

for(i in 1:length(msec_files)){
  filename <- paste0("Compiled_data/msec_out/", msec_files[i])
  x <- read.csv(filename, stringsAsFactors = F, head = T)
  wave_data <- rbind(wave_data, x)
}

colnames(wave_data)[which(colnames(wave_data) == "long")] <- "Longitude"
colnames(wave_data)[which(colnames(wave_data) == "lat")] <- "Latitude"
wave_data <- unique(wave_data)

write.csv(wave_data, "Compiled_data/msec_out_wave.csv", row.names = F)
