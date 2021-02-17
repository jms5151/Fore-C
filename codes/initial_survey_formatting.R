## Initial formatting of survey data

# load libraries ----------------------
library(tidyverse)
library(data.table) 
library(taxize)
library(stringr)

# load data ---------------------------
# NOAA Ecosystem Science Division data
load("Data/ESD2008_2012dataFOREC.Rdata")
esd0812 <- x 

load("Data/ESD2013_2017dataFOREC.Rdata")
esd1317 <- x

# Hawaii Coral Disease Database (HICORDIS) data
hicordis <- read.csv( "Data/hicordis.csv", head = T, stringsAsFactors = F)

# University of Guam data
guamRaw <- read.csv("Data/HEALTH_IMPACTS_DATABASE_FOR_NOAA.csv", stringsAsFactors = F, head = T)
guamCodes <- read.csv("Data/species_codes.csv", stringsAsFactors = F, head = T)
guam.gps <- read.csv("Data/guam_sites_with_gps.csv", stringsAsFactors = F, head = T)

# Great Barrier Reef (GBR) Reef Health and Impact Surveys (RHIS) data
gbr <- read.csv("Data/GBRMPA_RHIS_01012009-18092020.csv", stringsAsFactors = F, head = T)

# format ----------------------------
# column names - make first letter capitalized and all others lowercase
colnames(esd0812) <- str_to_title(colnames(esd0812))
colnames(esd1317) <- str_to_title(colnames(esd1317))
colnames(hicordis) <- str_to_title(colnames(hicordis))
colnames(guamRaw) <- str_to_title(colnames(guamRaw))
colnames(guam.gps) <- str_to_title(colnames(guam.gps))
colnames(gbr) <- str_to_title(colnames(gbr))

# dates
esd0812$Date <- as.Date(esd0812$Date_, "%d-%b-%y")
esd1317$Date <- as.Date(esd1317$Date_, "%d-%b-%y")
hicordis$Date <- as.Date(hicordis$Date, "%Y-%m-%d")
guamRaw$Date <- as.Date(guamRaw$Date, "%m/%d/%Y")
gbr$Date <- as.Date(gbr$Observation.date, "%m/%d/%Y")

# edit projects from HICRODIS data
hicordis <- subset(hicordis, Project != "CRED")
hicordis$Project[hicordis$Project == "Aeby" | hicordis$Project == "Kenyon"] <- "Aeby_Kenyon"

# add info to RHIS data
gbr$Region <- "GBR"
gbr$Island <- gbr$Marine.park.mgmt.section
gbr$Project <- "RHIS"

# format Guam colonies sizes as mid-point of each size class
guamRaw$Colony_size <- ifelse(guamRaw$Size_class == 1, 5, NA)
guamRaw$Colony_size <- ifelse(guamRaw$Size_class == 2, 20.5, guamRaw$Colony_size)
guamRaw$Colony_size <- ifelse(guamRaw$Size_class == 3, 45.5, guamRaw$Colony_size)
guamRaw$Colony_size <- ifelse(guamRaw$Size_class == 4, 80.5, guamRaw$Colony_size)
guamRaw$Colony_size <- ifelse(guamRaw$Size_class == 5, 150, guamRaw$Colony_size)
guamRaw$Colony_size <- ifelse(guamRaw$Size_class == 6, 350, guamRaw$Colony_size)

# fix errors -------------------------
# fix Guam coordinate error
xold <- guam.gps$Longitude[guam.gps$Site == "LUM"]
yold <- guam.gps$Latitude[guam.gps$Site == "LUM"]

guam.gps$Longitude[guam.gps$Site == "LUM"] <- yold
guam.gps$Latitude[guam.gps$Site == "LUM"] <- xold

# fix colony size issues
esd0812$Colonylength <- as.numeric(esd0812$Colonylength)
esd1317$Colonylength <- ifelse(esd1317$Colonylength < 0 | esd1317$Colonylength > 2000, NA, esd1317$Colonylength)

# add info to Guam data
guam <- guamRaw %>% 
  left_join(guam.gps, by = c("Site")) %>% 
  left_join(guamCodes, by = "Species_code")

# format taxonomy
acroporids <- children("Acroporidae", db = "worms") # list species by coral family indicated
poritids <- children("Poritidae", db = "worms")

esd0812$Family <- ifelse((esd0812$Genus %in% acroporids$Acroporidae$childtaxa_name) == TRUE, "Acroporidae", NA)
esd0812$Family <- ifelse((esd0812$Genus %in% poritids$Poritidae$childtaxa_name) == TRUE, "Poritidae", esd0812$Family)

esd1317$Family <- ifelse((esd1317$Genus %in% acroporids$Acroporidae$childtaxa_name) == TRUE, "Acroporidae", NA)
esd1317$Family <- ifelse((esd1317$Genus %in% poritids$Poritidae$childtaxa_name) == TRUE, "Poritidae", esd1317$Family)

hicordis$Family <- ifelse((hicordis$Genus %in% acroporids$Acroporidae$childtaxa_name) == TRUE, "Acroporidae", NA)
hicordis$Family <- ifelse((hicordis$Genus %in% poritids$Poritidae$childtaxa_name) == TRUE, "Poritidae", hicordis$Family)

guam$Genus <- gsub("([A-Za-z]+).*", "\\1", guam$Species)
guam$Family <- ifelse((guam$Genus %in% acroporids$Acroporidae$childtaxa_name) == TRUE, "Acroporidae", NA)
guam$Family <- ifelse((guam$Genus %in% poritids$Poritidae$childtaxa_name) == TRUE, "Poritidae", guam$Family)

