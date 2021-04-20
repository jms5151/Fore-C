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

cdz_rf_fun <- function(df){
  # split data into testing and training data
  sample <- sample.split(df$Y, SplitRatio = .70)
  train <- subset(df, sample == TRUE)
  test <- subset(df, sample == FALSE)
  # Perform training:
  rf_train <- randomForest(Y ~ ., data = df)
  # Perform testing:
  rf_test <- predict(rf_train, test)
  # assess fit
  rmse <- RMSE_forest(rf_test, test$Y)
  mae <- MAE_forest(rf_test, test$Y)
  # save all data
  newlist <- list(rf_train, rf_test, rmse, mae, test, train)
  return(newlist)
}

# 
# all_partial_plots <- function(df, rfModel){
# }
# 
# 
# all_partial_plots(ga_pac_rf[[6]], ga_pac_rf[[1]])


# GA Pacific -----------------------------------------------
ga_pac <- subset(GA_data, Region != "GBR" & Family == "Poritidae")
ga_pac <- ga_pac[, c("Y", "C", "Region", "Month", "Median_colony_size", "Poritidae_mean_cover", "H_abund", "wave_mean", "wave_sd", "SST_90dMean", "BlackMarble_2016_3km_geo.1")]
ga_pac <- ga_pac[complete.cases(ga_pac), ]

# potential number of variables to include before overfitting
log(nrow(ga_pac))

# check class of all variables
sapply(ga_pac, class)

# format some variables
ga_pac$Y <- as.integer(ga_pac$Y)
ga_pac$Region <- as.factor(ga_pac$Region) 
ga_pac$Month <- as.factor(ga_pac$Month) 

# run random forest model and assess fit
ga_pac_rf <- cdz_rf_fun(ga_pac)
varImpPlot(ga_pac_rf[[1]], main = "GA PACIFIC")
# as.data.frame(sort(importance(ga_pac_rf)[,1], decreasing = TRUE), optional = T) # print results from above plot
ga_pac_rf[[3]] # RMSE
ga_pac_rf[[4]] # MAE
plot_forest_accuracy(unlist(ga_pac_rf[2]), ga_pac_rf[[5]]$Y)

# partial dependence plots
par(mfrow=c(2, 3))
rfModel <- ga_pac_rf[[1]]
df <- ga_pac_rf[[6]]
variableNames <- colnames(df)
for(i in 2:length(variableNames)){
  partialPlot(rfModel, 
              pred.data = df, 
              x.var = variableNames[i], 
              main = variableNames[i], 
              xlab = variableNames[i])
}

# GA GBR ---------------------------------------------------
ga_gbr <- subset(GA_data, Region == "GBR")
ga_gbr <- ga_gbr[, c("Y", "Island", "Month", "Coral_cover", "Fish_abund", "wave_mean", "wave_sd")]
ga_gbr <- ga_gbr[complete.cases(ga_gbr), ]

# potential number of variables to include before overfitting
log(nrow(ga_gbr))

# check class of all variables
sapply(ga_gbr, class)

# format some variables
ga_gbr$Y <- as.integer(ga_gbr$Y)
ga_gbr$Island <- as.factor(ga_gbr$Island) 
ga_gbr$Month <- as.factor(ga_gbr$Month) 

# run random forest model and assess fit
ga_gbr_rf <- cdz_rf_fun(ga_gbr)
varImpPlot(ga_gbr_rf[[1]], main = "GA GBR")
ga_gbr_rf[[3]] # RMSE
ga_gbr_rf[[4]] # MAE
plot_forest_accuracy(unlist(ga_gbr_rf[2]), unlist(ga_gbr_rf[5]))

# partial dependence plots
rfModel <- ga_gbr_rf[[1]]
df <- ga_gbr_rf[[6]]
variableNames <- colnames(df)
for(i in 2:length(variableNames)){
  partialPlot(rfModel, 
              pred.data = df, 
              x.var = variableNames[i], 
              main = variableNames[i], 
              xlab = variableNames[i])
}

# WS GBR ---------------------------------------------------
ws_gbr <- subset(WS_data, Region == "GBR")
ws_gbr <- ws_gbr[, c("Y", "Island", "Month", "Coral_cover", "Fish_abund", "wave_mean", "wave_sd")]
ws_gbr <- ws_gbr[complete.cases(ws_gbr), ]

# potential number of variables to include before overfitting
log(nrow(ws_gbr))

# check class of all variables
sapply(ws_gbr, class)

# format some variables
ws_gbr$Island <- as.factor(ws_gbr$Island) 
ws_gbr$Month <- as.factor(ws_gbr$Month) 

# run random forest model and assess fit
ws_gbr_rf <- cdz_rf_fun(ws_gbr)
varImpPlot(ws_gbr_rf[[1]], main = "WS GBR")
ws_gbr_rf[[3]] # RMSE
ws_gbr_rf[[4]] # MAE
plot_forest_accuracy(unlist(ws_gbr_rf[2]), unlist(ws_gbr_rf[5]))

# partial dependence plots
rfModel <- ws_gbr_rf[[1]]
df <- ws_gbr_rf[[6]]
variableNames <- colnames(df)
for(i in 2:length(variableNames)){
  partialPlot(rfModel, 
              pred.data = df, 
              x.var = variableNames[i], 
              main = variableNames[i], 
              xlab = variableNames[i])
}

# WS Acropora Pacific ---------------------------------------------
ws_pac_acr <- subset(WS_data, Region != "GBR" & !is.na(Y) & Family == "Acroporidae")
ws_pac_acr <- ws_pac_acr[, c("Y", "C", "Region", "Month", "Median_colony_size", "H_abund",
                             "Parrotfish_abund", "Butterflyfish_abund",
                             "Acroporidae_mean_cover", "wave_mean", "wave_sd")]
ws_pac_acr <- ws_pac_acr[complete.cases(ws_pac_acr), ]

# potential number of variables to include before overfitting
log(nrow(ws_pac_acr))

# check class of all variables
sapply(ws_pac_acr, class)

# format some variables
ws_pac_acr$Region <- as.factor(ws_pac_acr$Region) 
ws_pac_acr$Month <- as.factor(ws_pac_acr$Month) 

# run random forest model and assess fit
ws_pac_acr_rf <- cdz_rf_fun(ws_pac_acr)
varImpPlot(ws_pac_acr_rf[[1]], main = "WS PACIFIC ACROPORA")
ws_pac_acr_rf[[3]] # RMSE
ws_pac_acr_rf[[4]] # MAE
plot_forest_accuracy(unlist(ws_pac_acr_rf[2]), unlist(ws_pac_acr_rf[5]))

# partial dependence plots
rfModel <- ws_pac_acr_rf[[1]]
df <- ws_pac_acr_rf[[6]]
variableNames <- colnames(df)
for(i in 2:length(variableNames)){
  partialPlot(rfModel, 
              pred.data = df, 
              x.var = variableNames[i], 
              main = variableNames[i], 
              xlab = variableNames[i])
}

# WS Porites Pacific ----------------------------------------------
ws_pac_por <- subset(WS_data, Region != "GBR" & Family == "Poritidae")
ws_pac_por <- ws_pac_por[, c("Y", "C", "Region", "Month", "Median_colony_size", "H_abund",
                             "Parrotfish_abund", "Butterflyfish_abund",
                             "Poritidae_mean_cover", "wave_mean", "wave_sd")]
ws_pac_por <- ws_pac_por[complete.cases(ws_pac_por), ]

# potential number of variables to include before overfitting
log(nrow(ws_pac_por))

# check class of all variables
sapply(ws_pac_por, class)

# format some variables
ws_pac_por$Region <- as.factor(ws_pac_por$Region) 
ws_pac_por$Month <- as.factor(ws_pac_por$Month) 

# run random forest model and assess fit
ws_pac_por_rf <- cdz_rf_fun(ws_pac_por)
varImpPlot(ws_pac_por_rf[[1]], main = "WS PACIFIC PORITES")
ws_pac_por_rf[[3]] # RMSE
ws_pac_por_rf[[4]] # MAE
plot_forest_accuracy(unlist(ws_pac_por_rf[2]), unlist(ws_pac_por_rf[5]))

# partial dependence plots
rfModel <- ws_pac_por_rf[[1]]
df <- ws_pac_por_rf[[6]]
variableNames <- colnames(df)
for(i in 2:length(variableNames)){
  partialPlot(rfModel, 
              pred.data = df, 
              x.var = variableNames[i], 
              main = variableNames[i], 
              xlab = variableNames[i])
}

# WS Pacific ----------------------------------------------
ws_pac <- subset(WS_data, Region != "GBR")
ws_pac <- ws_pac[, c("Y", "C", "Region", "Month", "Median_colony_size", "H_abund",
                             "Parrotfish_abund", "Butterflyfish_abund",
                             "Poritidae_mean_cover", "Acroporidae_mean_cover", "wave_mean", "wave_sd")]
ws_pac <- ws_pac[complete.cases(ws_pac), ]

# potential number of variables to include before overfitting
log(nrow(ws_pac))

# check class of all variables
sapply(ws_pac, class)

# format some variables
ws_pac$Region <- as.factor(ws_pac$Region) 
ws_pac$Month <- as.factor(ws_pac$Month) 

# run random forest model and assess fit
ws_pac_rf <- cdz_rf_fun(ws_pac)
varImpPlot(ws_pac_rf[[1]], main = "WS PACIFIC")
ws_pac_rf[[3]] # RMSE
ws_pac_rf[[4]] # MAE
plot_forest_accuracy(unlist(ws_pac_rf[2]), unlist(ws_pac_rf[5]))

# partial dependence plots
rfModel <- ws_pac_rf[[1]]
df <- ws_pac_rf[[6]]
variableNames <- colnames(df)
for(i in 2:length(variableNames)){
  partialPlot(rfModel, 
              pred.data = df, 
              x.var = variableNames[i], 
              main = variableNames[i], 
              xlab = variableNames[i])
}
