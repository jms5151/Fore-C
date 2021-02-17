# format data for bayesian models ---------------------------------------
rm(list=ls()) #remove previous variable assignments

# load data ---------------------------
source("codes/initial_survey_formatting.R")

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
            )

ws_esd1317 <- esd1317 %>%
  group_by(Date, Latitude, Longitude, Region, Island, Family) %>%
  summarize(Project = "NOAA", 
            C = length(Cond), 
            Y = sum(Cond == "TLS" | Cond == "WSY"), 
            p = Y/C, 
            Median_colony_size = median(Colonylength, na.rm = T),
            Coral_cover = NA
            )

# Hawaii coral disease database data
ws_hicordis <- hicordis %>%
  group_by(Date, Latitude, Longitude, Region, Island, Family, Project) %>%
  summarize(C = length(Disease_type), 
            Y = sum(Disease_type == "Tissue_Loss" | Disease_type == "White_Syndrome"), 
            p = Y/C, 
            Median_colony_size = median(Colony_length, na.rm = T),
            Coral_cover = NA
            )

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
            )

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
  filter(!is.na(Family)) %>% 
  filter(C >= minColNum)

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
            )

ga_esd1317 <- esd1317 %>%
  group_by(Date, Latitude, Longitude, Region, Island, Family) %>%
  summarize(Project = "NOAA", 
            C = length(Cond), 
            Y = sum(Cond == "SGA"), 
            p = Y/C,
            Median_colony_size = median(Colonylength, na.rm = T),
            Coral_cover = NA
            )

# Hawaii coral disease database data
ga_hicordis <- hicordis %>%
  group_by(Date, Latitude, Longitude, Region, Island, Family, Project) %>%
  summarize(C = length(Disease_type), 
            Y = sum(Ga == 1), 
            p = Y/C,
            Median_colony_size = median(Colony_length, na.rm = T),
            Coral_cover = NA
            )

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
            )

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
  filter(!is.na(Family)) %>% 
  filter(C >= minColNum)

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
  filter(Island == "Kauai")

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
            )
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
  filter(!is.na(Family)) %>% 
  filter(C >= minColNum)

# add ID
bbd$HS_ID <- seq(1, nrow(bbd), 1)
bbd$HS_ID <- paste0("BBD_", bbd$HS_ID)

# save data
save(bbd, file = "Compiled_data/BBD.RData")



# check frequency of disease observations
# WSfreq <- data.frame(table(gbr$WS))
# GAfreq <- data.frame(table(gbr$GA))
# BBDfreq <- data.frame(table(gbr$BBD))

# # save historical prevalence data for all diseases -----------------------------------------
# # combine data
# bbd_long_term_risk <- bbd[,c("Region", "Island", "p")] 
# ga_long_term_risk <- ga[,c("Region", "Island", "p")] 
# tls_long_term_risk <- tls[,c("Region", "Island", "p")]
# long_term_risk <- do.call("rbind", list(bbd_long_term_risk, ga_long_term_risk, tls_long_term_risk))
# 
# # format 
# colnames(long_term_risk)[3] <- "Prevalence"
# long_term_risk$Disease_type <- c(rep("BBD", nrow(bbd_long_term_risk)), rep("GA", nrow(ga_long_term_risk)), rep("TLS", nrow(tls_long_term_risk)))
# long_term_risk$Risk <- "Long term"
# long_term_risk$Region <- as.character(long_term_risk$Region)
# long_term_risk$Region[long_term_risk$Region=="MHI"|long_term_risk$Region=="NWHI"] <- "Hawaii"
# long_term_risk$Region[long_term_risk$Region=="MARIAN"] <- "Marianas"
# long_term_risk$Region[long_term_risk$Region=="SAMOA"] <- "Samoa"
# long_term_risk$Island[long_term_risk$Island == "French_Frigate_Shoals"] <- "French Frigate"
# long_term_risk$Island[long_term_risk$Island == "Pearl_and_Hermes"] <- "Pearl & Hermes"
# 
# # save data
# save(long_term_risk, file="Compiled_data/long_term_risk.RData")

# x <- subset(long_term_risk, Region == "Hawaii" & Disease_type == "GA")
# 
# ggplot(x, aes(x=Island, y=Prevalence)) + 
#   geom_boxplot() +
#   theme_classic() +
#   ylab("Prevalence") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ggplot(x) + 
#   geom_density_ridges(aes(x = Prevalence, y = Island), alpha = .3) +
#   theme_ridges() #+
#   ylab('') +
#   theme(axis.title.x = element_text(hjust=0.5)) +
#   scale_fill_manual(values = c("black", "deepskyblue3")) +
#   xlim(-0.01,0.15)
#   
# 
# hists <- subset(risk, Disease_type == "GA" & Risk == "nowcast")  
# ggplot(hists, aes(y = Region)) +
#     geom_density_ridges(aes(x = Prevalence, fill = Risk), alpha = .3) +
#     theme_ridges() +
#     ylab('') +
#     theme(axis.title.x = element_text(hjust=0.5)) +
#     scale_fill_manual(values = c("black", "deepskyblue3")) +
#     xlim(-0.01,0.15)

# Guam coral health surveys (line-intercept transect) and benthic surveys (belt transect)
# guamRaw <- read.csv("Guam_data/HEALTH_IMPACTS_DATABASE_FOR_NOAA.csv", stringsAsFactors = F, head=T)
# guamNOAA <- read.csv("Guam_data/LIT_WORKING_TABLE_RAW_DATA_FOR_NOAA.csv", stringsAsFactors = F, head=T)
# guamCoordinates <- read.csv("Guam_data/GPS_Coordinates.csv", stringsAsFactors = F, head=T)
# guam.gps <- read.csv("Guam_data/guam_sites_with_gps.csv", head=T)

# Calculate unique surveys and those that occurred prior to 2012
# esdpre12<- unique(esd0812[,c("LATITUDE", "LONGITUDE", "DATE_", "SITEVISITID", "REEF_ZONE")])
# esdpost12<- unique(esd1317[,c("LATITUDE", "LONGITUDE", "DATE_", "SITEVISITID", "REEF_ZONE")])
# hdz<- unique(hicordis[,c("Latitude", "Longitude", "Date", "Site")])
# hdz$Date <- as.Date(hdz$Date, "%Y-%m-%d")
# hdzpre12<- subset(hdz, Date < "2012-01-01")
# guam<- unique(guamRaw[,c("SITE", "DATE")])
# guam$DATE<- as.Date(guam$DATE, "%m/%d/%Y")
# guampre12<- subset(guam, DATE < "2012-01-01")
# 
# pre12 <- (nrow(esdpre12)+nrow(hdzpre12)+nrow(guampre12))
# all <- (nrow(esdpre12)+nrow(esdpost12)+nrow(hdz)+nrow(guam))
# pre12/all

# Characterize species affected by each disease 
# esd1counts <- ddply(esd0812, .(esd0812$TAXONNAME, esd0812$COND_DESCRIPTION), nrow)
# names(esd1counts) <- c("Species", "Condition", "Freq")
# 
# esd2counts <- ddply(esd1317, .(esd1317$SCIENTIFIC_NAME, esd1317$COND), nrow)
# names(esd2counts) <- c("Species", "Condition", "Freq")
# 
# hdzcounts <- ddply(hicordis, .(hicordis$Species, hicordis$Disease_Type), nrow)
# names(hdzcounts) <- c("Species", "Condition", "Freq")
# 
# counts <- rbind(esd1counts, esd2counts, hdzcounts)
# counts$Condition <- gsub("Acute Tissue Loss - White Syndrome|Sub-acute Tissue Loss|Tissue_Loss|Acute_Tissue_Loss|White_Syndrome|TLS|WSY", "Tissue_loss", counts$Condition)
# counts$Condition <- gsub("Skeletal Growth Anomalies|SGA", "Growth_anomalies", counts$Condition)
# counts <- subset(counts, Condition == "Tissue_loss"|Condition == "Growth_anomalies"|Condition == "Black_band_disease")
# counts <- subset(counts, Species != "NA")
# 
# # https://www.r-graph-gallery.com/79-levelplot-with-ggplot2.html
# library(ggplot2)
# 
# ggplot(counts, aes(Species, Condition, fill=Freq)) + 
#   geom_tile() +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# counts2 <- subset(counts, Freq > 100)
# 
# ggplot(counts2, aes(Species, Condition, fill=Freq)) + 
#   geom_tile() +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# counts_orig <- rbind(esd1counts, esd2counts, hdzcounts)
# x <- setdiff(counts_orig$Species, counts$Species)
# # 196 species affected
# # 169 species not affected