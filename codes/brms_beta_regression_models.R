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
load("Compiled_data/HI_grid_with_static_and_forecasted_covariates.RData")

HI_grid2 <- subset(HI_grid, EnsembleMember <= 5)

create_df <- function(grid, hotsnap){
  newdata <- data.frame(
    meanColSize = HI_grid2$Porites_MeanColonySize,
    HostCover = HI_grid2$Porites_mean_cover, # this is not actually the same!
    FishAbundance = HI_grid2$H_abund,
    Depth = HI_grid2$Porites_MeanDepth,
    HotSnap = HI_grid2[,hotsnap]
  )
}

summarise_results <- function(grid, predictions){
  summary_df <- cbind(grid, predictions)
  summary_df <- summary_df %>%
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

summarise_with_uncertainty <- function(grid, predictions, cast, disease){
  summary_df <- cbind(grid, predictions)
  summary_df <- summary_df %>%
    group_by(Island, Longitude, Latitude) %>%
    summarise(Estimate = median(Estimate),
              Q2.5 = quantile(Estimate, 0.025, na.rm = T),
              Q25 = quantile(Estimate, 0.25, na.rm = T),
              Q75 = quantile(Estimate, 0.75, na.rm = T),
              Q97.5 = quantile(Estimate, 0.975, na.rm = T))
  fileName <- paste0("Compiled_data/", cast, "/csv/", disease, "_", cast, "_risk_", substr(Sys.time(),1,10), ".RData")
  save(summary_df, file = fileName)
}

# Nowcast
nowcast <- create_df(HI_grid2, "Nowcast")
ga_nowcast_predictions <- predict(ga_fit, newdata = nowcast)
nowcast_df <- summarise_results(HI_grid2, ga_nowcast_predictions)
df_to_raster(nowcast_df, "nowcasts", "ga")
summarise_with_uncertainty(HI_grid2, ga_nowcast_predictions, "nowcasts", "ga")

# 4wk forecast
cast4 <- create_df(HI_grid2, "X4wkForecast")
ga_4wkcast_predictions <- predict(ga_fit, newdata = cast4)
cast4_df <- summarise_results(HI_grid2, ga_4wkcast_predictions)
df_to_raster(cast4_df, "forecasts_4wk", "ga")
summarise_with_uncertainty(HI_grid2, ga_4wkcast_predictions, "forecasts_4wk", "ga")

# 8wk forecast
cast8 <- create_df(HI_grid2, "X8wkForecast")
ga_8wkcast_predictions <- predict(ga_fit, newdata = cast8)
cast8_df <- summarise_results(HI_grid2, ga_8wkcast_predictions)
df_to_raster(cast8_df, "forecasts_8wk", "ga")
summarise_with_uncertainty(HI_grid2, ga_8wkcast_predictions, "forecasts_8wk", "ga")

# 12wk forecast
cast12 <- create_df(HI_grid2, "X12wkForecast")
ga_12wkcast_predictions <- predict(ga_fit, newdata = cast12)
cast12_df <- summarise_results(HI_grid2, ga_12wkcast_predictions)
df_to_raster(cast12_df, "forecasts_12wk", "ga")
summarise_with_uncertainty(HI_grid2, ga_12wkcast_predictions, "forecasts_12wk", "ga")

# virtual station summary
load("Compiled_data/nowcasts/csv/ga_nowcasts_risk_2020-11-17.RData")
nowcast <- summary_df

load("Compiled_data/forecasts_4wk/csv/ga_forecasts_4wk_risk_2020-11-17.RData")
cast4 <- summary_df

load("Compiled_data/forecasts_8wk/csv/ga_forecasts_8wk_risk_2020-11-17.RData")
cast8 <- summary_df

load("Compiled_data/forecasts_12wk/csv/ga_forecasts_12wk_risk_2020-11-17.RData")
cast12 <- summary_df

vs_summaries <- do.call("rbind", list(nowcast, cast4, cast8, cast12))
vs_summaries$Cast <- c(rep("Nowcast", nrow(nowcast)),
                       rep("Forecast-4wk", nrow(cast4)),
                       rep("Forecast-8wk", nrow(cast8)),
                       rep("Forecast-12wk", nrow(cast12)))

vs_summaries$VS <- as.character(vs_summaries$Island)
vs_summaries$VS[vs_summaries$VS == "Hawaii"] <- "Big Island"
vs_summaries$VS[vs_summaries$VS == "Maui"|vs_summaries$VS == "Molokai"|vs_summaries$VS == "Lanai"] <- "Maui - Molokai - Lanai - Kahoolawe"
vs_summaries$VS[vs_summaries$VS == "Kauai"|vs_summaries$VS == "Niihau"] <- "Kauai - Niihau"

vs_summary <- vs_summaries %>%
  group_by(Island, VS, Cast) %>%
  summarise_all(median, na.rm = T)

save(vs_summary, file = "VS_summary.RData")
