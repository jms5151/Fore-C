# load library
library(raster)

# load data
load("Compiled_data/Survey_points.RData")
r = raster("Data/GBR_LTMP_Fish-Herbiores/LTMP_Fish-Herbivores-Value.asc") # ; plot(r)
gbr <- read.csv("Data/GBRMPA_RHIS_01012009-18092020.csv", stringsAsFactors = F, head = T)

# extract fish data by survey points
fish <- extract(r, cbind(surveys$Longitude, surveys$Latitude))
fish_gbr <- cbind(surveys, "Fish_abund" = fish) 

# format coral cover data
gbr$Date <- as.Date(gbr$Observation.Date, "%m/%d/%Y")
gbr$coral_cover_r <- gbr$Total.Hard.Coral.. * rowSums(gbr[,c("Branching.Coral....Total.", "Plate.Table.Coral....Total.", "Vase.Foliose.Coral....Total.")])/100 
gbr$coral_cover_K <- gbr$Total.Hard.Coral.. * rowSums(gbr[,c("Encrusting.Coral....Total.", "Massive.Coral....Total.")])/100 
gbr_cov <- gbr[,c("Program.ID", "coral_cover_r", "coral_cover_K")] # actually program.ID won't link up, so not quite correct

# save data
save(fish_gbr, file = "Compiled_data/Fish_gbr.RData")
save(gbr_cov, file = "Compiled_data/Benthic_cover_gbr.RData")
