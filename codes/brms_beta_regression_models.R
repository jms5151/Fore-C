# load data
tls <- read.csv("Data/TLS_cc.csv")
ga <- read.csv("Data/GA_cc.csv")

# Constrain all values between 0 and 1
tls$TLS[tls$TLS == 0] <- 0.00001
tls$TLS[tls$TLS == 1] <- 0.99999

ga$GA[ga$GA == 0] <- 0.00001
ga$GA[ga$GA == 1] <- 0.99999

# load library
library(brms)

# run models
# tissue loss - porites
# tls_fit <- brm(TLS ~ meanColSize + HostDensity + HotSnap + Depth + WaveExposure + Population,
#                data = tls,
#                family = "beta")

# tissue loss - montipora ### replaced StreamExposure with effluent for forecasting
tls_fit <- brm(TLS ~ meanColSize + FishAbundance + scale(Effluent) + WaveExposure, # could replace stream exposure with + HotSnap 
               data = tls, 
               family = "beta")

# print output
print(summary(tls_fit))
plot(tls_fit2)

# predictions with simulated data
newdata <- data.frame(
  meanColSize = rnorm(100, 30, 10),
  FishAbundance = rnorm(100, mean(tls$FishAbundance), 0.03),
  StreamExposure = rnorm(100, mean(tls$StreamExposure), 80),
  WaveExposure = rnorm(100, mean(tls$WaveExposure), 10)
  # ,HotSnap = rnorm(100, mean(tls$Hotsnap), 0.05)
)

tls_predictions <- predict(tls_fit, newdata = newdata)

# growth anomalies
ga_fit <- brm(GA ~ meanColSize + HostCover + FishAbundance + HotSnap + Depth,
               data = ga, 
               family = "beta")

# print output
print(summary(ga_fit))

plot(ga_fit)

# predict for nowcast and forecasts
library(dplyr)
library(raster)
library(RANN)
library(tidyr)

#load data
load("Compiled_data/HI_grid_with_static_and_forecasted_covariates.RData")

meanColSize = seq(0, round(max(HI_grid$Porites_MeanColonySize)*2), 5)
HostCover = seq(0, round(max(HI_grid$Porites_mean_cover)*2), 3)
FishAbundance = seq(0, round(max(HI_grid$H_abund)*2), 0.1)
Depth = seq(0, round(max(HI_grid$Porites_MeanDepth)*2), 5)
HotSnap = c(seq(0, 2, 0.2), seq(3, round(max(HI_grid$Nowcast, HI_grid$X4wkForecast, HI_grid$X8wkForecast, HI_grid$X12wkForecast, na.rm = T)*1.5), 1))

newdata <- expand.grid("meanColSize" = meanColSize, 
                       "HostCover" = HostCover, 
                       "FishAbundance" = FishAbundance,
                       "Depth" = Depth,
                       "HotSnap" = HotSnap,
                       KEEP.OUT.ATTRS = T)


ga_predictions <- predict(ga_fit, newdata = newdata)
save(ga_predictions, file = "Compiled_data/HI_ga_predictions.RData")

# find GA values given different conditions
lookupTable <- cbind(newdata, ga_predictions)
# save(lookupTable, file = "Compiled_data/HI_lookupTable.RData")
HI_grid$meanColSize <- HI_grid$Porites_MeanColonySize
HI_grid$HostCover <- HI_grid$Porites_mean_cover
HI_grid$FishAbundance <- HI_grid$H_abund
HI_grid$Depth <- HI_grid$Porites_MeanDepth

castPred <- function(gridData, HS_name){
  gridData[, "HotSnap"] = gridData[, HS_name]
  gridData <- gridData[complete.cases(gridData),]
  nearTable <- as.data.frame(nn2(data = subset(lookupTable, select = c(meanColSize, HostCover, FishAbundance, Depth, HotSnap)), 
                                 query = subset(gridData, select = c(meanColSize, HostCover, FishAbundance, Depth, HotSnap)), 
                                 k = 1))
  gridData$Estimate <- lookupTable$Estimate[nearTable$nn.idx]
  gridData[,c("Island", "Longitude", "Latitude", "Estimate")] 
}

# create_df <- function(grid, hotsnap){
  newdata <- data.frame(
    meanColSize = HI_grid2$Porites_MeanColonySize,
    HostCover = HI_grid2$Porites_mean_cover, # this is not actually the same!
    FishAbundance = HI_grid2$H_abund,
    Depth = HI_grid2$Porites_MeanDepth,
    HotSnap = HI_grid2[,hotsnap]
  )
}

summarise_results <- function(predictions){
  summary_df <- predictions %>%
    group_by(Longitude, Latitude) %>%
    summarise(Estimate = median(Estimate))
}

df_to_raster <- function(grid, cast, disease){
  # create raster from point data
  grid <- rasterFromXYZ(grid)
  # set projection
  crs(grid) <- CRS("+init=epsg:4326")
  # save data
  fileName <- paste0("Compiled_data/", cast, "/raster/", substr(Sys.time(),1,10), "_", disease, ".tif")
  writeRaster(grid, file=fileName)
}

summarise_with_uncertainty <- function(predictions, cast, disease){
  summary_df <- predictions %>%
    group_by(Island, Longitude, Latitude) %>%
    summarise(Estimate = median(Estimate),
              Q2.5 = quantile(Estimate, 0.025, na.rm = T),
              Q25 = quantile(Estimate, 0.25, na.rm = T),
              Q75 = quantile(Estimate, 0.75, na.rm = T),
              Q97.5 = quantile(Estimate, 0.975, na.rm = T))
  fileName <- paste0("Compiled_data/", cast, "/csv/", disease, "_", cast, "_risk_", substr(Sys.time(),1,10), ".RData")
  save(summary_df, file = fileName)
}

pred_8wkcast <- castPred(HI_grid, "X4wkForecast")
pred_8wkcast <- castPred(HI_grid, "X4wkForecast")

# Nowcast
pred_nowcast <- castPred(HI_grid, "Nowcast")
nowcast_df <- summarise_results(pred_nowcast)
df_to_raster(nowcast_df, "nowcasts", "ga")
summarise_with_uncertainty(pred_nowcast, "nowcasts", "ga")

# 4wk forecast
pred_4wkcast <- castPred(HI_grid, "X4wkForecast")
cast4_df <- summarise_results(pred_4wkcast)
df_to_raster(cast4_df, "forecasts_4wk", "ga")
summarise_with_uncertainty(pred_4wkcast, "forecasts_4wk", "ga")

# 8wk forecast
pred_8wkcast <- castPred(HI_grid, "X8wkForecast")
cast8_df <- summarise_results(pred_8wkcast)
df_to_raster(cast8_df, "forecasts_8wk", "ga")
summarise_with_uncertainty(pred_8wkcast, "forecasts_8wk", "ga")

# 12wk forecast
pred_12wkcast <- castPred(HI_grid, "X12wkForecast")
cast12_df <- summarise_results(pred_12wkcast)
df_to_raster(cast12_df, "forecasts_12wk", "ga")
summarise_with_uncertainty(pred_12wkcast, "forecasts_12wk", "ga")

# virtual station summaries with scenarios
# create scenario values
HI_grid$VS <- as.character(HI_grid$Island)
HI_grid$VS[HI_grid$VS == "Hawaii"] <- "Big Island"
HI_grid$VS[HI_grid$VS == "Maui"|HI_grid$VS == "Molokai"|HI_grid$VS == "Lanai"|HI_grid$VS == "Kaula"] <- "Maui - Molokai - Lanai - Kahoolawe"
HI_grid$VS[HI_grid$VS == "Kauai"|HI_grid$VS == "Niihau"] <- "Kauai - Niihau"

vs_summary <- HI_grid %>%
  group_by(VS) %>%
  summarise(
    meanColSize = mean(Porites_MeanColonySize),
    HostCover = mean(Porites_mean_cover),
    FishAbundance = mean(H_abund),
    Depth = mean(Porites_MeanDepth),
    Nowcast = mean(Nowcast, na.rm = T),
    X4wkForecast = mean(X4wkForecast, na.rm = T),
    X8wkForecast = mean(X8wkForecast, na.rm = T),
    X12wkForecast = mean(X12wkForecast, na.rm = T)
  ) 

scenario_values <- c(seq(-50, -10, 10), 100, seq(10, 50, 10))
scenario_df <- expand.grid("VS" = unique(vs_summary$VS),
                           "meanColSize_condition" = scenario_values,
                           "HostCover_condition" = scenario_values,
                           "FishAbundance_condition" = scenario_values,
                           "Depth_condition" = scenario_values,
                           "HotSnap_condition" = scenario_values)

vs_summary_wide <- merge(vs_summary, scenario_df, by = "VS", all.y = T)

delta_value <- function(df, colname, colcondition){
  df[, colname] + df[, colcondition]/100 * df[, colname]
}

vs_summary_wide$meanColSize <- delta_value(vs_summary_wide, "meanColSize", "meanColSize_condition")
vs_summary_wide$HostCover <- delta_value(vs_summary_wide, "HostCover", "HostCover_condition")
vs_summary_wide$FishAbundance <- delta_value(vs_summary_wide, "FishAbundance", "FishAbundance_condition")
vs_summary_wide$Depth <- delta_value(vs_summary_wide, "Depth", "Depth_condition")
vs_summary_wide$Nowcast <- delta_value(vs_summary_wide, "Nowcast", "HotSnap_condition")
vs_summary_wide$X4wkForecast <- delta_value(vs_summary_wide, "X4wkForecast", "HotSnap_condition")
vs_summary_wide$X8wkForecast <- delta_value(vs_summary_wide, "X8wkForecast", "HotSnap_condition")
vs_summary_wide$X12wkForecast <- delta_value(vs_summary_wide, "X12wkForecast", "HotSnap_condition")

vs_summary_wide[vs_summary_wide == 100] <- 0

# Get estimates from lookup table
# load("Compiled_data/HI_lookupTable.RData")

castScenarios <- function(gridData, HS_name, castName){
  gridData[, "HotSnap"] = gridData[, HS_name]
  gridData <- gridData[complete.cases(gridData),]
  nearTable <- as.data.frame(nn2(data = subset(lookupTable, select = c(meanColSize, HostCover, FishAbundance, Depth, HotSnap)), 
                                 query = subset(gridData, select = c(meanColSize, HostCover, FishAbundance, Depth, HotSnap)), 
                                 k = 1))
  gridData[paste0(castName, "_Estimate")] <- lookupTable$Estimate[nearTable$nn.idx]
  gridData[paste0(castName, "_Q2.5")] <- lookupTable$Q2.5[nearTable$nn.idx]
  gridData[paste0(castName, "_Q97.5")] <- lookupTable$Q97.5[nearTable$nn.idx]
  gridData
}

nowcast_scenarios <- castScenarios(vs_summary_wide, "Nowcast", "Nowcast")
forecast4_scenarios <- castScenarios(vs_summary_wide, "X4wkForecast", "X4wkForecast")
forecast8_scenarios <- castScenarios(vs_summary_wide, "X8wkForecast", "X8wkForecast")
forecast12_scenarios <- castScenarios(vs_summary_wide, "X12wkForecast", "X12wkForecast")

vs_scenarios <- cbind(nowcast_scenarios, forecast4_scenarios[, 16:18])
vs_scenarios <- cbind(vs_scenarios, forecast8_scenarios[, 16:18])
vs_scenarios <- cbind(vs_scenarios, forecast12_scenarios[, 16:18])

save(vs_scenarios, file = "compiled_data/vs_scenarios_wide.RData")

vs_long <- vs_scenarios %>%
  gather(Cast, value, Nowcast_Estimate:X12wkForecast_Q97.5) %>% 
  separate(Cast, c("Cast", "Type"), sep = "_") %>% 
  spread(Type, value)

cfsDate <- as.Date("2020-09-26", "%Y-%m-%d")
vs_long$Date <- cfsDate
vs_long$Date[vs_long$Cast == "X4wkForecast"] <- cfsDate + 30
vs_long$Date[vs_long$Cast == "X8wkForecast"] <- cfsDate + 60  
vs_long$Date[vs_long$Cast == "X12wkForecast"] <- cfsDate + 90

save(vs_long, file = "Compiled_data/vs_scenarios_long.RData")

# determine risk estimates for all combinations of scenario values
# load("Compiled_data/nowcasts/csv/ga_nowcasts_risk_2020-11-17.RData")
# nowcast <- summary_df
# 
# load("Compiled_data/forecasts_4wk/csv/ga_forecasts_4wk_risk_2020-11-17.RData")
# cast4 <- summary_df
# 
# load("Compiled_data/forecasts_8wk/csv/ga_forecasts_8wk_risk_2020-11-17.RData")
# cast8 <- summary_df
# 
# load("Compiled_data/forecasts_12wk/csv/ga_forecasts_12wk_risk_2020-11-17.RData")
# cast12 <- summary_df
# 
# vs_summaries <- do.call("rbind", list(nowcast, cast4, cast8, cast12))
# vs_summaries$Cast <- c(rep("Nowcast", nrow(nowcast)),
#                        rep("Forecast-4wk", nrow(cast4)),
#                        rep("Forecast-8wk", nrow(cast8)),
#                        rep("Forecast-12wk", nrow(cast12)))
# 
# 
# vs_summary <- vs_summaries %>%
#   group_by(Island, VS, Cast) %>%
#   summarise_all(median, na.rm = T)
# 
# save(vs_summary, file = "VS_summary.RData")
