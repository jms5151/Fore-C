library(randomForest)
library(caTools)

# load data
load("Compiled_data/GA_with_predictors.RData")
load("Compiled_data/WS_with_predictors.RData")

# custom functions
RMSE_forest <- function(predictionForecst, testOutcome){
  sqrt(mean((predictionForecst - testOutcome)^2))
}

MAE_forest <- function(predictionForecst, testOutcome){
  mean(abs(predictionForecst - testOutcome))
}

plot_forest_accuracy <- function(predictionForecst, testOutcome){
  maxXY <- max(testOutcome, predictionForecst)
  plot(testOutcome, predictionForecst, pch = 16, ylim = c(0, maxXY), xlim = c(0, maxXY), ylab = "Predicted Y", xlab = "Observed Y", main = "Validation set")
}

custom_cdz_rf_fun <- function(df){
  # split data into testing and training data
  sample <- sample.split(df$Y, SplitRatio = .70)
  train <- subset(df, sample == TRUE)
  test <- subset(df, sample == FALSE)
  # Perform training:
  rf_train <- randomForest(Y ~ ., data = df, importance = T)
  # Perform testing:
  rf_test <- predict(rf_train, test)
  rmse <- RMSE_forest(rf_test, test$Y)
  mae <- MAE_forest(rf_test, test$Y)
  newlist <- list(rf_train, rf_test, rmse, mae, test$Y)
  return(newlist)
}

# GA Pacific -----------------------------------------------
ga_pac <- subset(GA_data, Region != "GBR" & Family == "Poritidae")
ga_pac <- ga_pac[, c("Y", 
                     "C", 
                     "Region",
                     "Month",
                     "Median_colony_size",
                     "Poritidae_mean_cover",
                     "H_abund",
                     "wave_mean",
                     "wave_sd",
                     "SST_90dMean",
                     "Winter_condition",
                     "Hot_snaps",
                     "cold_snaps",
                     "BlackMarble_2016_3km_geo.1",
                     "BlackMarble_2016_3km_geo.2",
                     "BlackMarble_2016_3km_geo.3",
                     "Long_Term_Chl_Median",
                     "Long_Term_Chl_90th",
                     "Long_Term_Kd_Median",
                     "Long_Term_Kd_90th",
                     "Long_Term_Chl_Variability",
                     "Long_Term_Kd_Variability",
                     "Long_Term_Median_Residual",
                     "Three_Week_Chl_Median",
                     "Three_Week_Kd_Median",
                     "Three_Week_Chl_Variability",
                     "Three_Week_Kd_Variability",
                     "Three_Week_Median_Residual",
                     "Acute_Chla_4week",
                     "Acute_Kd_4week",
                     "Acute_Chla_8week",
                     "Acute_Kd_8week",
                     "Acute_Chla_12week",
                     "Acute_Kd_12week"
                     )]
ga_pac <- ga_pac[complete.cases(ga_pac), ]

# potential number of variables to include before overfitting
log(nrow(ga_pac))

# check class of all variables
sapply(ga_pac, class)

# format some variables
ga_pac$Y <- as.integer(ga_pac$Y)
ga_pac$Region <- as.factor(ga_pac$Region) 
ga_pac$Month <- as.factor(ga_pac$Month) 

# assess fit
ga_pac_rf <- custom_cdz_rf_fun(ga_pac)
# use top from %IncMSE, it is the increase in MSE of predictions 
# (estimated with out-of-bag-CV) as a result of variable j being 
# permuted (values randomly shuffled).
# IncNodePurity is based on the Gini index, indicating inequality 
# among values 
varImpPlot(ga_pac_rf[[1]], main = "GA PACIFIC")
# as.data.frame(sort(importance(ga_pac_rf)[,1], decreasing = TRUE), optional = T) # print results from above plot
ga_pac_rf[[3]] # RMSE
ga_pac_rf[[4]] # MAE
plot_forest_accuracy(unlist(ga_pac_rf[2]), unlist(ga_pac_rf[5]))

# plot partial dependence plots for each variable
ga_pac_imp <- importance(ga_pac_rf[[1]])
ga_pac_impvar <- rownames(ga_pac_imp)[order(ga_pac_imp[, 1], decreasing=TRUE)]
ga_pac_impvar <- ga_pac_impvar[1:16]
op <- par(mfrow=c(4, 4))
for (i in seq_along(ga_pac_impvar)) {
  partialPlot(ga_pac_rf[[1]], as.data.frame(ga_pac), ga_pac_impvar[i], xlab=ga_pac_impvar[i],
              main=paste("GA Pac PDP\n", ga_pac_impvar[i]))
}

# GA GBR ---------------------------------------------------
ga_gbr <- subset(GA_data, Region == "GBR")
ga_gbr <- ga_gbr[, c("Y", 
                     "Island", 
                     "Month", 
                     "Coral_cover", 
                     "Fish_abund", 
                     "wave_mean", 
                     "wave_sd", 
                     "SST_90dMean",
                     "Winter_condition", 
                     "Hot_snaps", 
                     "cold_snaps",
                     "BlackMarble_2016_3km_geo.2",
                     "BlackMarble_2016_3km_geo.3",
                     "Long_Term_Chl_Median",
                     "Long_Term_Chl_90th",
                     "Long_Term_Kd_Median",
                     "Long_Term_Kd_90th",
                     "Long_Term_Chl_Variability",
                     "Long_Term_Kd_Variability",
                     "Long_Term_Median_Residual",
                     "Three_Week_Chl_Median",
                     "Three_Week_Kd_Median",
                     "Three_Week_Chl_Variability",
                     "Three_Week_Kd_Variability",
                     "Three_Week_Median_Residual",
                     "Acute_Chla_4week",
                     "Acute_Kd_4week",
                     "Acute_Chla_8week",
                     "Acute_Kd_8week",
                     "Acute_Chla_12week",
                     "Acute_Kd_12week"
                     )]
ga_gbr <- ga_gbr[complete.cases(ga_gbr), ]

# potential number of variables to include before overfitting
log(nrow(ga_gbr))

# check class of all variables
sapply(ga_gbr, class)

# format some variables
ga_gbr$Y <- as.integer(ga_gbr$Y)
ga_gbr$Island <- as.factor(ga_gbr$Island) 
ga_gbr$Month <- as.factor(ga_gbr$Month) 

# assess fit
ga_gbr_rf <- custom_cdz_rf_fun(ga_gbr)
varImpPlot(ga_gbr_rf[[1]], main = "GA GBR")
ga_gbr_rf[[3]] # RMSE
ga_gbr_rf[[4]] # MAE
plot_forest_accuracy(unlist(ga_gbr_rf[2]), unlist(ga_gbr_rf[5]))

# plot partial dependence plots for each variable
ga_gbr_imp <- importance(ga_gbr_rf[[1]])
ga_gbr_impvar <- rownames(ga_gbr_imp)[order(ga_gbr_imp[, 1], decreasing=TRUE)]
ga_gbr_impvar <- ga_gbr_impvar[1:16]
op <- par(mfrow=c(4, 4))
for (i in seq_along(ga_gbr_impvar)) {
  partialPlot(ga_gbr_rf[[1]], as.data.frame(ga_gbr), ga_gbr_impvar[i], xlab=ga_gbr_impvar[i],
              main=paste("GA gbr PDP\n", ga_gbr_impvar[i]))
}

# WS GBR ---------------------------------------------------
ws_gbr <- subset(WS_data, Region == "GBR")
ws_gbr <- ws_gbr[, c("Y", 
                     "Island", 
                     "Month", 
                     "Coral_cover", 
                     "Fish_abund", 
                     "wave_mean", 
                     "wave_sd", 
                     "Winter_condition", 
                     "Hot_snaps", 
                     "cold_snaps",
                     "Long_Term_Chl_Median",
                     "Long_Term_Chl_90th",
                     "Long_Term_Kd_Median",
                     "Long_Term_Kd_90th",
                     "Long_Term_Chl_Variability",
                     "Long_Term_Kd_Variability",
                     "Long_Term_Median_Residual",
                     "Three_Week_Chl_Median",
                     "Three_Week_Kd_Median",
                     "Three_Week_Chl_Variability",
                     "Three_Week_Kd_Variability",
                     "Three_Week_Median_Residual",
                     "Acute_Chla_4week",
                     "Acute_Kd_4week",
                     "Acute_Chla_8week",
                     "Acute_Kd_8week",
                     "Acute_Chla_12week",
                     "Acute_Kd_12week"
                     )]
ws_gbr <- ws_gbr[complete.cases(ws_gbr), ]

# potential number of variables to include before overfitting
log(nrow(ws_gbr))

# check class of all variables
sapply(ws_gbr, class)

# format some variables
ws_gbr$Island <- as.factor(ws_gbr$Island) 
ws_gbr$Month <- as.factor(ws_gbr$Month) 

# assess fit
ws_gbr_rf <- custom_cdz_rf_fun(ws_gbr)
varImpPlot(ws_gbr_rf[[1]], main = "WS GBR")
ws_gbr_rf[[3]] # RMSE
ws_gbr_rf[[4]] # MAE
plot_forest_accuracy(unlist(ws_gbr_rf[2]), unlist(ws_gbr_rf[5]))

# plot partial dependence plots for each variable
ws_gbr_imp <- importance(ws_gbr_rf[[1]])
ws_gbr_impvar <- rownames(ws_gbr_imp)[order(ws_gbr_imp[, 1], decreasing=TRUE)]
ws_gbr_impvar <- ws_gbr_impvar[1:16]
op <- par(mfrow=c(4, 4))
for (i in seq_along(ws_gbr_impvar)) {
  partialPlot(ws_gbr_rf[[1]], ws_gbr, ws_gbr_impvar[i], xlab=ws_gbr_impvar[i],
              main=paste("ws gbr PDP\n", ws_gbr_impvar[i]))
}

# WS acropora all other regions ------------------------------------
# format data
ws_pac_acr <- subset(WS_data, Region != "GBR" & !is.na(Y) & Family == "Acroporidae")
ws_pac_acr <- ws_pac_acr[, c("Y", 
                             "C", 
                             "Region",
                             "Month",
                             "Median_colony_size",
                             "H_abund",
                             "Parrotfish_abund",
                             "Butterflyfish_abund",
                             "Acroporidae_mean_cover",
                             "wave_mean",
                             "wave_sd",
                             "Winter_condition",
                             "Hot_snaps",
                             "cold_snaps",
                             "Long_Term_Chl_Median",
                             "Long_Term_Chl_90th",
                             "Long_Term_Kd_Median",
                             "Long_Term_Kd_90th",
                             "Long_Term_Chl_Variability",
                             "Long_Term_Kd_Variability",
                             "Long_Term_Median_Residual",
                             "Three_Week_Chl_Median",
                             "Three_Week_Kd_Median",
                             "Three_Week_Chl_Variability",
                             "Three_Week_Kd_Variability",
                             "Three_Week_Median_Residual",
                             "Acute_Chla_4week",
                             "Acute_Kd_4week",
                             "Acute_Chla_8week",
                             "Acute_Kd_8week",
                             "Acute_Chla_12week",
                             "Acute_Kd_12week"
                             )]
ws_pac_acr <- ws_pac_acr[complete.cases(ws_pac_acr), ]

# potential number of variables to include before overfitting
log(nrow(ws_gbr))

# check class of all variables
sapply(ws_gbr, class)

# format some variables
ws_pac_acr$Month <- as.factor(ws_pac_acr$Month) 
ws_pac_acr$Region <- as.factor(ws_pac_acr$Region) 

# assess fit
ws_pac_acr_rf <- custom_cdz_rf_fun(ws_pac_acr)
varImpPlot(ws_pac_acr_rf[[1]], main = "WS pac_acr")
ws_pac_acr_rf[[3]] # RMSE
ws_pac_acr_rf[[4]] # MAE
plot_forest_accuracy(unlist(ws_pac_acr_rf[2]), unlist(ws_pac_acr_rf[5]))

# plot partial dependence plots for each variable
ws_pac_acr_imp <- importance(ws_pac_acr_rf[[1]])
ws_pac_acr_impvar <- rownames(ws_pac_acr_imp)[order(ws_pac_acr_imp[, 1], decreasing=TRUE)]
ws_pac_acr_impvar <- ws_pac_acr_impvar[1:16]
op <- par(mfrow=c(4, 4))
for (i in seq_along(ws_pac_acr_impvar)) {
  partialPlot(ws_pac_acr_rf[[1]], ws_pac_acr, ws_pac_acr_impvar[i], xlab=ws_pac_acr_impvar[i],
              main=paste("ws pac_acr PDP\n", ws_pac_acr_impvar[i]))
}

# WS porites all other regions ------------------------------------
# format data
ws_pac_por <- subset(WS_data, Region != "GBR" & Family == "Poritidae")
ws_pac_por <- ws_pac_por[, c("Y", 
                             "C", 
                             "Region", 
                             "Month", 
                             "Median_colony_size", 
                             "H_abund",
                             "Parrotfish_abund", 
                             "Butterflyfish_abund",
                             "Poritidae_mean_cover", 
                             "wave_mean", 
                             "wave_sd", 
                             "Winter_condition", 
                             "Hot_snaps", 
                             "cold_snaps",
                             "Long_Term_Chl_Median",
                             "Long_Term_Chl_90th",
                             "Long_Term_Kd_Median",
                             "Long_Term_Kd_90th",
                             "Long_Term_Chl_Variability",
                             "Long_Term_Kd_Variability",
                             "Long_Term_Median_Residual",
                             "Three_Week_Chl_Median",
                             "Three_Week_Kd_Median",
                             "Three_Week_Chl_Variability",
                             "Three_Week_Kd_Variability",
                             "Three_Week_Median_Residual",
                             "Acute_Chla_4week",
                             "Acute_Kd_4week",
                             "Acute_Chla_8week",
                             "Acute_Kd_8week",
                             "Acute_Chla_12week",
                             "Acute_Kd_12week"
                             )]
ws_pac_por <- ws_pac_por[complete.cases(ws_pac_por), ]

# potential number of variables to include before overfitting
log(nrow(ws_pac_por))

# check class of all variables
sapply(ws_pac_por, class)

# format some variables
ws_pac_por$Month <- as.factor(ws_pac_por$Month) 
ws_pac_por$Region <- as.factor(ws_pac_por$Region) 

# assess fit
ws_pac_por_rf <- custom_cdz_rf_fun(ws_pac_por)
varImpPlot(ws_pac_por_rf[[1]], main = "WS pac_por")
ws_pac_por_rf[[3]] # RMSE
ws_pac_por_rf[[4]] # MAE
plot_forest_accuracy(unlist(ws_pac_por_rf[2]), unlist(ws_pac_por_rf[5]))

# plot partial dependence plots for each variable
ws_pac_por_imp <- importance(ws_pac_por_rf[[1]])
ws_pac_por_impvar <- rownames(ws_pac_por_imp)[order(ws_pac_por_imp[, 1], decreasing=TRUE)]
ws_pac_por_impvar <- ws_pac_por_impvar[1:16]
op <- par(mfrow=c(4, 4))
for (i in seq_along(ws_pac_por_impvar)) {
  partialPlot(ws_pac_por_rf[[1]], ws_pac_por, ws_pac_por_impvar[i], xlab=ws_pac_por_impvar[i],
              main=paste("ws pac_por PDP\n", ws_pac_por_impvar[i]))
}
