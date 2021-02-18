# format data for bayesian models ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data ---------------------------
source("codes/initial_survey_formatting.R")
load("Compiled_data/median_colony_sizes_by_island.RData")

# vector of column names
final_column_names <- c("Date", "Latitude", "Longitude", "Region", "Island", "Family", "Project", "C", "Y", "p", "Median_colony_size", "Coral_cover")

# minimum number of host colonies per transect
minColNum <- 5

# White syndromes ------------------------------ 
# NOAA Ecosystem Science Division data
ws_esd0812 <- esd0812 %>% 
  group_by(Date, Latitude, Longitude, Region, Island, Family) %>%
  summarize(Project = "NOAA", 
            C = length(Cond_description), 
            Y = sum(Cond_description == "Sub-acute Tissue Loss" | Cond_description == "Acute Tissue Loss - White Syndrome"), 
            p = Y/C, 
            Median_colony_size = median(Colonylength, na.rm = T),
            Coral_cover = NA
            ) %>% 
  filter(C >= minColNum)

ws_esd1317 <- esd1317 %>%
  group_by(Date, Latitude, Longitude, Region, Island, Family) %>%
  summarize(Project = "NOAA", 
            C = length(Cond), 
            Y = sum(Cond == "TLS" | Cond == "WSY"), 
            p = Y/C, 
            Median_colony_size = median(Colonylength, na.rm = T),
            Coral_cover = NA
            ) %>% 
  filter(C >= minColNum)

# Hawaii coral disease database data
ws_hicordis <- hicordis %>%
  group_by(Date, Latitude, Longitude, Region, Island, Family, Project) %>%
  summarize(C = length(Disease_type), 
            Y = sum(Disease_type == "Tissue_Loss" | Disease_type == "White_Syndrome"), 
            p = Y/C, 
            Median_colony_size = median(Colony_length, na.rm = T),
            Coral_cover = NA
            ) %>% 
  filter(C >= minColNum)

# Guam data 
ws_guam <- guam %>%
  group_by(Date, Latitude, Longitude, Family) %>%
  summarize(Region = "MARIAN", 
            Island = "Guam", 
            Project = "University of Guam", 
            C = length(Species), 
            Y = sum(Tl == "TL" | Ws == "WS"), 
            p = Y/C, 
            Median_colony_size = median(Colony_size, na.rm = T),
            Coral_cover = NA
            ) %>% 
  filter(C >= minColNum)

ws_guam <- ws_guam[, final_column_names] # reorder for combining below

# GBR RHIS data
gbr$C <- NA
gbr$p <- NA
gbr$Median_colony_size <- NA

# Maybe only use plate/table for now, and otherwise everything together
gbr$Y <- gbr$White.syndromes..Plate.table.
gbr$Family <- "Acroporidae"
gbr$Coral_cover <- gbr$Plate.table.coral....Total.
ws_gbr <- gbr[, final_column_names]
# gbr$Y <- rowSums(gbr[, c("White.Syndromes..Branching.", "White.Syndromes..Plate.Table.", "White.Syndromes..Vase.Foliose.")])
# gbr$Family <- "Short-lived morphologies"
# ws_gbr_r <- gbr[,c("Date", "Latitude", "Longitude", "Region", "Island", "Family", "Project", "C", "Y", "p")]
# 
# gbr$Y <- rowSums(gbr[, c("White.Syndromes..Encrusting.", "White.Syndromes..Massive.")])
# gbr$Family <- "Long-lived morphologies"
# ws_gbr_K <- gbr[,c("Date", "Latitude", "Longitude", "Region", "Island", "Family", "Project", "C", "Y", "p")]
# 
# ws_gbr <- rbind(ws_gbr_r, ws_gbr_K)

# merge data
ws <- rbindlist(list(ws_esd0812, ws_esd1317, ws_hicordis, ws_guam, ws_gbr))

# filter 
ws <- ws %>% 
  left_join(median_sizes, by = c("Island", "Family")) %>%
  filter(!is.na(Family))

# add island level colony median size if NA 
ws$Median_colony_size <- ifelse(is.na(ws$Median_colony_size) == T, ws$Island_median_colony_size, ws$Median_colony_size)
ws$Island_median_colony_size <- NULL

# add ID
ws$HS_ID <- seq(1, nrow(ws), 1)
ws$HS_ID <- paste0("WS_", ws$HS_ID)

# save data
save(ws, file = "Compiled_data/WS.RData")

# growth anomalies ------------------------------------------ 
# NOAA Ecosystem Science Division data
ga_esd0812 <- esd0812 %>% 
  group_by(Date, Latitude, Longitude, Region, Island, Family) %>%
  summarize(Project = "NOAA", 
            C = length(Cond_description), 
            Y = sum(Cond_description == "Skeletal Growth Anomalies"), 
            p = Y/C,
            Median_colony_size = median(Colonylength, na.rm = T),
            Coral_cover = NA
            ) %>% 
  filter(C >= minColNum)

ga_esd1317 <- esd1317 %>%
  group_by(Date, Latitude, Longitude, Region, Island, Family) %>%
  summarize(Project = "NOAA", 
            C = length(Cond), 
            Y = sum(Cond == "SGA"), 
            p = Y/C,
            Median_colony_size = median(Colonylength, na.rm = T),
            Coral_cover = NA
            ) %>% 
  filter(C >= minColNum)

# Hawaii coral disease database data
ga_hicordis <- hicordis %>%
  group_by(Date, Latitude, Longitude, Region, Island, Family, Project) %>%
  summarize(C = length(Disease_type), 
            Y = sum(Ga == 1), 
            p = Y/C,
            Median_colony_size = median(Colony_length, na.rm = T),
            Coral_cover = NA
            ) %>% 
  filter(C >= minColNum)

# Guam surveys 
ga_guam <- guam %>%
  group_by(Date, Latitude, Longitude, Family) %>%
  summarize(Region = "MARIAN", 
            Island = "Guam", 
            Project = "University of Guam", 
            C = length(Species), 
            Y = sum(Ga == "GA"), 
            p = Y/C,
            Median_colony_size = median(Colony_size, na.rm = T),
            Coral_cover = NA
            ) %>% 
  filter(C >= minColNum)

ga_guam <- ga_guam[, final_column_names]

# RHIS surveys
gbr$Y <- gbr$Other.disease..Tumors.. 
gbr$Family <- "All morphologies combined"
gbr$Median_colony_size <- NA
gbr$Coral_cover <- gbr$Live.coral
ga_gbr <- gbr[, final_column_names]

# merge data
ga <- rbindlist(list(ga_esd0812, ga_esd1317, ga_hicordis, ga_guam, ga_gbr))

# filter 
ga <- ga %>% 
  left_join(median_sizes, by = c("Island", "Family")) %>%
  filter(!is.na(Family))

# add island level colony median size if NA 
ga$Median_colony_size <- ifelse(is.na(ga$Median_colony_size) == T, ga$Island_median_colony_size, ga$Median_colony_size)
ga$Island_median_colony_size <- NULL

# add ID
ga$HS_ID <- seq(1, nrow(ga), 1)
ga$HS_ID <- paste0("GA_", ga$HS_ID)

# save data
save(ga, file = "Compiled_data/GA.RData")

# Black band disease -------------------------------------------- 
# Hawaii coral disease database data
bbd_hicordis <- hicordis %>%
  group_by(Date, Latitude, Longitude, Region, Island, Family, Project) %>%
  summarize(C = length(Disease_type), 
            Y = sum(Disease_type == "Black_band_disease"), 
            p = Y/C,
            Median_colony_size = median(Colony_length, na.rm = T),
            Coral_cover = NA
            ) %>%
  filter(Island == "Kauai") %>% 
  filter(C >= minColNum)

# Guam surveys 
bbd_guam <- guam %>%
  group_by(Date, Latitude, Longitude, Family) %>%
  summarize(Region = "MARIAN", 
            Island = "Guam", 
            Project = "University of Guam", 
            C = length(Species), 
            Y = sum(Bbd == "BBD"), 
            p = Y/C,
            Median_colony_size = median(Colony_size, na.rm = T),
            Coral_cover = NA
            ) %>% 
  filter(C >= minColNum)
bbd_guam <- bbd_guam[, final_column_names] # reorder for combining below

# GBR surveys
bbdNames <- names(gbr[grep("^Black.band",names(gbr))])
gbr$Y <- rowSums(gbr[,bbdNames[2:length(bbdNames)]])
gbr$Family <- "All morphologies combined"
bbd_gbr <- gbr[, final_column_names]

# merge data
bbd <- rbindlist(list(bbd_hicordis, bbd_guam, bbd_gbr))

# filter 
bbd <- bbd %>% 
  filter(!is.na(Family))

# add ID
bbd$HS_ID <- seq(1, nrow(bbd), 1)
bbd$HS_ID <- paste0("BBD_", bbd$HS_ID)

# save data
save(bbd, file = "Compiled_data/BBD.RData")
