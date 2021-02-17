# combine all disease data -----------------------------------------------------------------

# load data
# source("codes/format_survey_data_for_bayes_models.R")
# OR
load("Compiled_data/BBD.RData")
load("Compiled_data/GA.RData")
load("Compiled_data/WS.RData")

observations <- rbindlist(list(bbd, ga, ws))

# format columns
colnames(observations)[which(colnames(observations) == "p")] <- "Prevalence"
observations$Disease_type <- c(rep("BBD", nrow(bbd)), rep("GA", nrow(ga)), rep("WS", nrow(ws)))
observations$Year <- format(observations$Date, "%Y")

# format region
observations$Region <- as.character(observations$Region)
observations$Region[observations$Region=="MHI"|observations$Region=="NWHI"] <- "Hawaii"
observations$Region[observations$Region=="MARIAN"] <- "Marianas"
observations$Region[observations$Region=="SAMOA"] <- "Samoa"

# format island
observations$Island <- as.character(observations$Island)
observations$Island[observations$Island == "French_Frigate_Shoals"] <- "French Frigate"
observations$Island[observations$Island == "Pearl_and_Hermes"] <- "Pearl & Hermes"
observations$Island <- ifelse(observations$Region == "GBR", "", observations$Island)

# format data source
observations$Project[observations$Project=="Aeby_Kenyon"|observations$Project=="Burns"|observations$Project=="Caldwell"|observations$Project=="Kenyon"|observations$Project=="Ross"|observations$Project=="Runyon"|observations$Project=="Walsh"|observations$Project=="Walton"] <- "University of Hawaii"
observations$Project[observations$Project=="Couch"] <- "Cornell University"
observations$Project[observations$Project=="White"] <- "Hawaii Division of Aquatic Resources"
observations$Project[observations$Project=="CRED"] <- "NOAA"

# save
save(observations, file="Compiled_data/observational_data.RData")
