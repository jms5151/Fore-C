# ZIB
# https://avehtari.github.io/modelselection/roaches.html
# https://bookdown.org/ajkurz/DBDA_recoded/model-comparison-and-hierarchical-modeling.htmlhttps://bookdown.org/ajkurz/DBDA_recoded/model-comparison-and-hierarchical-modeling.html

# Model comparisons  -----------------------------------------------------------------------------

# load libraries
library(brms)
library(bayesplot)

# load data
load("Compiled_data/GA_with_predictors.RData")
load("Compiled_data/WS_with_predictors.RData")

# GA Pacific -----------------------------------------------
ga_pac <- subset(GA_data, Region != "GBR" & Family == "Poritidae")
ga_pac <- ga_pac[, c("Y", "C", "Region", "Month", "Median_colony_size", "Poritidae_mean_cover", "H_abund", "wave_mean", "wave_sd", "SST_90dMean", "BlackMarble_2016_3km_geo.1")]
ga_pac <- ga_pac[complete.cases(ga_pac), ]

gaPac_SizeFishSST <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                     # scale(Poritidae_mean_cover) +
                                     scale(H_abund) +
                                     # scale(wave_mean) +
                                     scale(SST_90dMean) #+
                                     # Region +
                                     # Month
                                   ),
                                data = ga_pac,
                                family = zero_inflated_binomial(),
                                chains = 2)

conditional_effects(gaPac_SizeFishSST)
summary(gaPac_SizeFishSST)

saveRDS(gaPac_SizeFishSST, file = "Compiled_data/model_objects/gaPac_SizeFishSST.Rds")

# Make dataset to predict on
medColSize = seq(min(ga_pac$Median_colony_size), max(ga_pac$Median_colony_size)*1.5, 10)
FishAbundance = seq(0.01, round(max(ga_pac$H_abund)*2), 0.1)
SST = seq(round(min(ga_pac$SST_90dMean)-3), round(max(ga_pac$SST_90dMean)+3), 0.5)

newdata <- expand.grid("Median_colony_size" = medColSize,
                       "H_abund" = FishAbundance,
                       "SST_90dMean" = SST,
                       KEEP.OUT.ATTRS = T)

# library(msm)
# test <- rtnorm(n = nrow(newdata), mean = mean(ga_pac$C), sd = sd(ga_pac$C[ga_pac$C<1500]), lower = 5, upper = Inf)
# hist(test)
newdata$C <- rnbinom(n = nrow(newdata), size = 2, mu = mean(ga_pac$C))
# newdata$C <- round(rlnorm(n = nrow(newdata), mean = mean(log(ga_pac$C)), sd = sd(log(ga_pac$C[ga_pac$C<1500]))))
sum(newdata$C < 5)
newdata$C[newdata$C < 5] <- 5
# plot(newdata$Colonies)
# hist(newdata$Colonies)
# hist(ga_pac$C)
prev <- mean(ga_pac$Y/ga_pac$C)
newdata$Y <- rbinom(n = nrow(newdata), size = newdata$C, prob = prev) 
range(newdata$Y)
# hist(newdata$diseasedColonies)           
# hist(ga_pac$Y)                 

gaPac_SizeFishSST <- readRDS("Compiled_data/model_objects/gaPac_SizeFishSST.Rds")
ga_predictions <- predict(gaPac_SizeFishSST, newdata = newdata, nsamples = 2000)

save(ga_predictions, file = "Compiled_data/ga_pacific_predictions.RData")


# ----------------
gaPac_SizeCovWaveMean <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                              scale(Poritidae_mean_cover) +
                              # scale(H_abund) +
                              scale(wave_mean) #+
                              # scale(SST_90dMean) +
                              # Region +
                            # Month
                            ),
                         data = ga_pac,
                         family = zero_inflated_binomial(),
                         chains = 2)

conditional_effects(gaPac_SizeCovWaveMean)
summary(gaPac_SizeCovWaveMean)

saveRDS(gaPac_SizeCovWaveMean, file = "Compiled_data/model_objects/gaPac_SizeCovWaveMean.Rds")

gaPac_SizeRegionMonth <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                Region +
                                Month),
                             data = ga_pac,
                             family = zero_inflated_binomial(),
                             chains = 2)

conditional_effects(gaPac_SizeRegionMonth)
summary(gaPac_SizeRegionMonth)

saveRDS(gaPac_SizeRegionMonth, file = "Compiled_data/model_objects/gaPac_SizeRegionMonth.Rds")


options(mc.cores=4)
gaPac_SizeFishCovWaveMean <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                             scale(H_abund) + 
                                             scale(Poritidae_mean_cover) +
                                             scale(wave_mean)
                                ),
                             data = ga_pac,
                             family = zero_inflated_binomial(),
                             chains = 2)

conditional_effects(gaPac_SizeFishCovWaveMean)
summary(gaPac_SizeFishCovWaveMean)

saveRDS(gaPac_SizeFishCovWaveMean, file = "Compiled_data/model_objects/gaPac_SizeFishCovWaveMean.Rds")


options(mc.cores=6)
gaPac_SizeFishCovWaveMeanSSTRegion <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                  scale(H_abund) +
                                  scale(Poritidae_mean_cover) +
                                  scale(wave_mean) +
                                  scale(SST_90dMean) +
                                  Region
                                ),
                             data = ga_pac,
                             family = zero_inflated_binomial(),
                             chains = 2)

conditional_effects(gaPac_SizeFishCovWaveMeanSSTRegion)
summary(gaPac_SizeFishCovWaveMeanSSTRegion)

saveRDS(gaPac_SizeFishCovWaveMeanSSTRegion, file = "Compiled_data/model_objects/gaPac_SizeFishCovWaveMeanSSTRegion.Rds")

model_weights(gaPac_SizeFishSST, gaPac_SizeCovWaveMean, gaPac_SizeRegionMonth, gaPac_SizeFishCovWaveMean, gaPac_SizeFishCovWaveMeanSSTRegion,
              weights = "loo")

options(mc.cores=7)
gaPac_SizeFishCovWaveMeanSSTMonth <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                                    scale(H_abund) +
                                                    scale(Poritidae_mean_cover) +
                                                    scale(wave_mean) +
                                                    scale(SST_90dMean) +
                                                    Month
                                               ),
                                               data = ga_pac,
                                               family = zero_inflated_binomial(),
                                               chains = 2)

conditional_effects(gaPac_SizeFishCovWaveMeanSSTMonth)
summary(gaPac_SizeFishCovWaveMeanSSTMonth)

saveRDS(gaPac_SizeFishCovWaveMeanSSTMonth, file = "Compiled_data/model_objects/gaPac_SizeFishCovWaveMeanSSTMonth.Rds")

model_weights(gaPac_SizeFishCovWaveMeanSSTRegion, gaPac_SizeFishCovWaveMeanSSTMonth, weights = "loo")

gaPac_SizeFishCovWaveSDSSTMonth <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                              scale(H_abund) +
                                              scale(Poritidae_mean_cover) +
                                              scale(wave_sd) +
                                              scale(SST_90dMean) +
                                              Month
                                          ),
                                       data = ga_pac,
                                       family = zero_inflated_binomial(),
                                       chains = 2)

conditional_effects(gaPac_SizeFishCovWaveSDSSTMonth)
summary(gaPac_SizeFishCovWaveSDSSTMonth)

saveRDS(gaPac_SizeFishCovWaveSDSSTMonth, file = "Compiled_data/model_objects/gaPac_SizeFishCovWaveSDSSTMonth.Rds")
model_weights(gaPac_SizeFishCovWaveMeanSSTMonth, gaPac_SizeFishCovWaveSDSSTMonth, weights = "loo")

gaPac_SizeFishCovWaveSDSSTRegion <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                            scale(H_abund) +
                                            scale(Poritidae_mean_cover) +
                                            scale(wave_sd) +
                                            scale(SST_90dMean) +
                                            Region
                                           ),
                                        data = ga_pac,
                                        family = zero_inflated_binomial(),
                                        chains = 2)

conditional_effects(gaPac_SizeFishCovWaveSDSSTRegion)
summary(gaPac_SizeFishCovWaveSDSSTRegion)

saveRDS(gaPac_SizeFishCovWaveSDSSTRegion, file = "Compiled_data/model_objects/gaPac_SizeFishCovWaveSDSSTRegion.Rds")
model_weights(gaPac_SizeFishCovWaveSDSSTRegion, gaPac_SizeFishCovWaveSDSSTMonth, weights = "loo")

gaPac_SizeFishWaveSDSSTMonthNight <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                             scale(H_abund) +
                                             scale(BlackMarble_2016_3km_geo.1) +
                                             scale(wave_sd) +
                                             scale(SST_90dMean) +
                                             Month
                                            ),
                                         data = ga_pac,
                                         family = zero_inflated_binomial(),
                                         chains = 2)

conditional_effects(gaPac_SizeFishWaveSDSSTMonthNight)
summary(gaPac_SizeFishWaveSDSSTMonthNight)

saveRDS(gaPac_SizeFishWaveSDSSTMonthNight, file = "Compiled_data/model_objects/gaPac_SizeFishWaveSDSSTMonthNight.Rds")
model_weights(gaPac_SizeFishWaveSDSSTMonthNight, gaPac_SizeFishCovWaveSDSSTMonth, weights = "loo")

# WS GBR ---------------------------------------------------
ws_gbr <- subset(WS_data, Region == "GBR")
ws_gbr <- ws_gbr[, c("Y", "Island", "Month", "Coral_cover", "Fish_abund", "wave_mean", "wave_sd")]
ws_gbr <- ws_gbr[complete.cases(ws_gbr), ]


ws_gbr_waveMean <- brm(bf(Y ~ scale(Coral_cover) + scale(Fish_abund) + scale(wave_mean) + Island + Month,
                     zi ~ Island + Month),
                  family = zero_inflated_negbinomial(), 
                  data = ws_gbr,
                  chains = 3,
                  prior=set_prior("normal(0,1)"), refresh=500)

saveRDS(ws_gbr_waveMean, file = "Compiled_data/model_objects/ws_gbr_full_mod_waveMean.Rds")

ws_gbr_waveSD <- brm(bf(Y ~ scale(Coral_cover) + scale(Fish_abund) + scale(wave_sd) + Island + Month,
                          zi ~ Island + Month),
                     family = zero_inflated_negbinomial(),
                     data = ws_gbr,
                     chains = 3,
                     prior=set_prior("normal(0,1)"), refresh=500)

saveRDS(ws_gbr_waveSD, file = "Compiled_data/model_objects/ws_gbr_full_mod_waveSD.Rds")

# WS acropora all other regions ------------------------------------
# format data
ws_pac_acr <- subset(WS_data, Region != "GBR" & !is.na(Y) & Family == "Acroporidae")
ws_pac_acr <- ws_pac_acr[, c("Y", "C", "Region", "Month", "Median_colony_size", "H_abund",
                             "Parrotfish_abund", "Butterflyfish_abund",
                             "Acroporidae_mean_cover", "wave_mean", "wave_sd")]
ws_pac_acr <- ws_pac_acr[complete.cases(ws_pac_acr), ]

# compare base models
wsPacAcr_regionFixed <- brm(bf(Y|trials(C) ~ Region), data = ws_pac_acr, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacAcr_regionFixed, file = "Compiled_data/model_objects/wsPacAcr_regionFixed.Rds")
wsPacAcr_regionZI <- brm(bf(Y|trials(C) ~ 1,  zi ~ Region), data = ws_pac_acr, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacAcr_regionZI, file = "Compiled_data/model_objects/wsPacAcr_regionZI.Rds")
wsPacAcr_regionFixedZI <- brm(bf(Y|trials(C) ~ Region,  zi ~ Region), data = ws_pac_acr, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacAcr_regionFixedZI, file = "Compiled_data/model_objects/wsPacAcr_regionFixedZI.Rds")

wsPacAcr_monthFixed <- brm(bf(Y|trials(C) ~ Month), data = ws_pac_acr, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacAcr_monthFixed, file = "Compiled_data/model_objects/wsPacAcr_monthFixed.Rds")
wsPacAcr_monthZI <- brm(bf(Y|trials(C) ~ 1,  zi ~ Month), data = ws_pac_acr, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacAcr_monthZI, file = "Compiled_data/model_objects/wsPacAcr_monthZI.Rds")
wsPacAcr_monthFixedZI <- brm(bf(Y|trials(C) ~ Month,  zi ~ Month), data = ws_pac_acr, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacAcr_monthFixedZI, file = "Compiled_data/model_objects/wsPacAcr_monthFixedZI.Rds")

wsPacAcr_regionMonthFixed <- brm(bf(Y|trials(C) ~ Region + Month), data = ws_pac_acr, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacAcr_regionMonthFixed, file = "Compiled_data/model_objects/wsPacAcr_regionMonthFixed.Rds")
wsPacAcr_regionMonthZI <- brm(bf(Y|trials(C) ~ 1,  zi ~ Region + Month), data = ws_pac_acr, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacAcr_regionMonthZI, file = "Compiled_data/model_objects/wsPacAcr_regionMonthZI.Rds")
wsPacAcr_regionMonthFixedZI <- brm(bf(Y|trials(C) ~ Region + Month,  zi ~ Region + Month), data = ws_pac_acr, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacAcr_regionMonthFixedZI, file = "Compiled_data/model_objects/wsPacAcr_regionMonthFixedZI.Rds")
# ESS too low on this last one

x <- list.files("Compiled_data/model_objects/")

for(i in 1:length(x)){
  modName <- substr(x[i], 1, nchar(x[i])-4)
  fileName <- paste0("Compiled_data/model_objects/", modName, ".Rds")
  y <- readRDS(fileName)
  assign(modName, y)
}

# compare models
# fit_b1 <- add_criterion(fit_b1, "waic")
# fit_b4 <- add_criterion(fit_b4, "waic")
# loo_compare(fit_b1, fit_b4, criterion = "waic") # best fitting model appears on top (value == 0)
model_weights(wsPacAcr_regionFixed, wsPacAcr_regionZI, wsPacAcr_regionFixedZI,
              # wsPacAcr_monthFixed, wsPacAcr_monthZI, 
              wsPacAcr_monthFixedZI,
              wsPacAcr_regionMonthFixed, wsPacAcr_regionMonthZI, wsPacAcr_regionMonthFixedZI,
              weights = "loo") # higher value best model, divide by others and get how many times better

# fish comparison (H_abund best)

wsPacAcr_Habund <- brm(bf(Y|trials(C) ~ scale(H_abund)),
                                data = ws_pac_acr,
                                family = zero_inflated_binomial(),
                                chains = 3)
saveRDS(wsPacAcr_Habund, file = "Compiled_data/model_objects/wsPacAcr_Habund.Rds")

wsPacAcr_parrotfish <- brm(bf(Y|trials(C) ~ scale(Parrotfish_abund)),
                                data = ws_pac_acr,
                                family = zero_inflated_binomial(),
                                chains = 3)
saveRDS(wsPacAcr_parrotfish, file = "Compiled_data/model_objects/wsPacAcr_parrotfish.Rds")

wsPacAcr_butterflyfish <- brm(bf(Y|trials(C) ~ scale(Butterflyfish_abund)),
                                data = ws_pac_acr,
                                family = zero_inflated_binomial(),
                                chains = 3)
saveRDS(wsPacAcr_butterflyfish, file = "Compiled_data/model_objects/wsPacAcr_butterflyfish.Rds")

model_weights(wsPacAcr_Habund, wsPacAcr_parrotfish, wsPacAcr_butterflyfish, weights = "loo")

# wave comparison (wave sd is best)
wsPacAcr_waveMean <- brm(bf(Y|trials(C) ~ scale(wave_mean)),
                           data = ws_pac_acr,
                           family = zero_inflated_binomial(),
                           chains = 3)
saveRDS(wsPacAcr_waveMean, file = "Compiled_data/model_objects/wsPacAcr_waveMean.Rds")

wsPacAcr_waveSD <- brm(bf(Y|trials(C) ~ scale(wave_sd)),
                              data = ws_pac_acr,
                              family = zero_inflated_binomial(),
                              chains = 3)
saveRDS(wsPacAcr_waveSD, file = "Compiled_data/model_objects/wsPacAcr_waveSD.Rds")

model_weights(wsPacAcr_waveMean, wsPacAcr_waveSD, weights = "loo")

# wsPacAcr_Habund_waveMean <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                   #   scale(H_abund) +
                                   #   scale(Acroporidae_mean_cover) +
                                   #   scale(wave_mean) +
                                     # Region +
                                     # Month
                                     # ,
                                   # zi ~ Region + Month
                         #        ), 
                         # data = ws_pac_acr, 
                         # family = zero_inflated_binomial(), 
                         # chains = 2)
conditional_effects(wsPacAcr_Habund_waveMean)
summary(wsPacAcr_Habund_waveMean)

saveRDS(wsPacAcr_Habund_waveMean, file = "Compiled_data/model_objects/wsPacAcr_fullMod_Habund_waveMean.Rds")




# WS porites all other regions ------------------------------------
# format data
ws_pac_por <- subset(WS_data, Region != "GBR" & Family == "Poritidae")
ws_pac_por <- ws_pac_por[, c("Y", "C", "Region", "Month", "Median_colony_size", "H_abund",
                             # "Parrotfish_abund", "Butterflyfish_abund",
                             "Poritidae_mean_cover", "wave_mean", "wave_sd")]
ws_pac_por <- ws_pac_por[complete.cases(ws_pac_por), ]

wsPacPor_regionMonthFixed <- brm(bf(Y|trials(C) ~ Region + Month), data = ws_pac_por, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacPor_regionMonthFixed, file = "Compiled_data/model_objects/wsPacPor_regionMonthFixed.Rds")
summary(wsPacPor_regionMonthFixed)
conditional_effects(wsPacPor_regionMonthFixed)

wsPacPor_regionMonthZI <- brm(bf(Y|trials(C) ~ 1,  zi ~ Region + Month), data = ws_pac_por, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacPor_regionMonthZI, file = "Compiled_data/model_objects/wsPacPor_regionMonthZI.Rds")
summary(wsPacPor_regionMonthZI)
conditional_effects(wsPacPor_regionMonthZI)

wsPacPor_regionMonthFixedZI <- brm(bf(Y|trials(C) ~ Region + Month,  zi ~ Region + Month), data = ws_pac_por, family = zero_inflated_binomial(), chains = 3)
saveRDS(wsPacPor_regionMonthFixedZI, file = "Compiled_data/model_objects/wsPacPor_regionMonthFixedZI.Rds")
summary(wsPacPor_regionMonthFixedZI)
conditional_effects(wsPacPor_regionMonthFixedZI)

model_weights(wsPacPor_regionMonthFixed, wsPacPor_regionMonthZI, wsPacPor_regionMonthFixedZI, weights = "loo")

# wave comparison (wave mean is best)
wsPacPor_waveMean <- brm(bf(Y|trials(C) ~ scale(wave_mean)),
                         data = ws_pac_por,
                         family = zero_inflated_binomial(),
                         chains = 3)
saveRDS(wsPacPor_waveMean, file = "Compiled_data/model_objects/wsPacPor_waveMean.Rds")

wsPacPor_waveSD <- brm(bf(Y|trials(C) ~ scale(wave_sd)),
                       data = ws_pac_por,
                       family = zero_inflated_binomial(),
                       chains = 3)
saveRDS(wsPacPor_waveSD, file = "Compiled_data/model_objects/wsPacPor_waveSD.Rds")

model_weights(wsPacPor_waveMean, wsPacPor_waveSD, weights = "loo")

model_weights(wsPacPor_waveMean, wsPacPor_regionMonthFixedZI, weights = "loo")


# lots of covars
wsPacPor_SizeFishCoverWaveMean <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                             scale(H_abund) +
                             scale(Acroporidae_mean_cover) +
                             scale(wave_mean)),
                       data = ws_pac_por,
                       family = zero_inflated_binomial(),
                       chains = 3)
saveRDS(wsPacPor_SizeFishCoverWaveMean, file = "Compiled_data/model_objects/wsPacPor_SizeFishCoverWaveMean.Rds")

wsPacPor_SizeFishCoverWaveSD <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                           scale(H_abund) +
                                           scale(Acroporidae_mean_cover) +
                                           scale(wave_sd)),
                                      data = ws_pac_por,
                                      family = zero_inflated_binomial(),
                                      chains = 3)
saveRDS(wsPacPor_SizeFishCoverWaveSD, file = "Compiled_data/model_objects/wsPacPor_SizeFishCoverWaveSD.Rds")

model_weights(wsPacPor_SizeFishCoverWaveMean, wsPacPor_SizeFishCoverWaveSD, weights = "loo")


wsPacPor_SizeFishCoverWaveMeanRegion <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                           scale(H_abund) +
                                           scale(Acroporidae_mean_cover) +
                                           scale(wave_mean) +
                                           Region),
                                      data = ws_pac_por,
                                      family = zero_inflated_binomial(),
                                      chains = 3)
saveRDS(wsPacPor_SizeFishCoverWaveMeanRegion, file = "Compiled_data/model_objects/wsPacPor_SizeFishCoverWaveMeanRegion.Rds")

wsPacPor_SizeFishCoverWaveMeanMonth <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                                 scale(H_abund) +
                                                 scale(Acroporidae_mean_cover) +
                                                 scale(wave_mean) +
                                                 Month),
                                            data = ws_pac_por,
                                            family = zero_inflated_binomial(),
                                            chains = 3)
saveRDS(wsPacPor_SizeFishCoverWaveMeanMonth, file = "Compiled_data/model_objects/wsPacPor_SizeFishCoverWaveMeanMonth.Rds")

model_weights(wsPacPor_SizeFishCoverWaveMeanRegion, wsPacPor_SizeFishCoverWaveMeanMonth, weights = "loo")

# I think this one can't run
wsPacPor_SizeFishCoverWaveMeanMonthRegion <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                                scale(H_abund) +
                                                scale(Acroporidae_mean_cover) +
                                                scale(wave_mean) +
                                                Month +
                                                Region),
                                           data = ws_pac_por,
                                           family = zero_inflated_binomial(),
                                           chains = 3)
saveRDS(wsPacPor_SizeFishCoverWaveMeanMonthRegion, file = "Compiled_data/model_objects/wsPacPor_SizeFishCoverWaveMeanMonthRegion.Rds")




wsPacPor_SizeFishCoverWaveMean <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                                           scale(H_abund) +
                                           scale(Acroporidae_mean_cover) +
                                           scale(wave_mean) #+
                                         #   Month +
                                         #   Region,
                                         # zi ~ Month + Region
),
data = ws_pac_por,
family = zero_inflated_binomial(),
chains = 3)

# -------------------------------------------------------------------------------------
pos <- subset(ws_gbr, Y > 0)
neg <- subset(ws_gbr, Y == 0)
neg2 <- neg[sample(nrow(neg), nrow(pos)),]
ws2 <- rbind(pos, neg2)
ws3 <- ws2[sample(nrow(ws2), 250),]

fit_zinb3 <- brm(Y ~ JulDay,  data = ws2, family = zero_inflated_poisson())

summary(fit_zinb3)
plot(conditional_effects(fit_zinb3), ask = FALSE)

fit_zinb4 <- brm(Y ~ JulDay,  data = ws3, family = zero_inflated_negbinomial())
summary(fit_zinb4)
plot(conditional_effects(fit_zinb4), ask = FALSE)

# maybe the zi part should fit to sin(Julian day) rather than a random effect
fit_zinb5 <- brm(Y ~ Island,  data = ws3, family = zero_inflated_poisson())
summary(fit_zinb5)
plot(conditional_effects(fit_zinb5), ask = FALSE)


fit_zinb6 <- brm(bf(Y ~ a * sin(b * JulDay), a + b ~ 1, nl = TRUE),
                 data = ws2, prior = priors, refresh = 0)
marginal_effects(fit_zinb6)

fit_zinb6.2 <- brm(bf(Y ~ a * sin(b * JulDay), a + b ~ 1, nl = TRUE),
                   data = ws3, prior = priors, refresh = 0)
marginal_effects(fit_zinb6.2)

fit_zinb7 <- brm(Y ~ Month, data = ws_gbr, family = zero_inflated_poisson())
marginal_effects(fit_zinb7)

fit_zinb8 <- brm(bf(Y ~ Month, zi ~ Month), data = ws3, family = zero_inflated_poisson())
marginal_effects(fit_zinb8)


fit_b1 <- brm(bf(Y|trials(C) ~ Month + Region,  zi ~ Month + Region), data = ws_o, family = zero_inflated_binomial(), chains = 2)
summary(fit_b1)
plot(conditional_effects(fit_b1), ask = FALSE)
yrepnzb <- posterior_predict(fit_b1)
(prop_zero_test4 <- ppc_stat(y=ws_o$Y, yrepnzb, stat=function(y) mean(y==0)))
plot(fit_b1)
(max_test_znb <- ppc_stat(y=ws_o$Y, yrepnzb, stat="max"))
(loop <- loo(fit_b1))
plot(loop)
pp_check(fit_b1)
pp_check(fit_b1, type="bars", nsamples = 100)
prop_zero <- function(x) {sum(x == 0)/length(x)}
ppc_stat(y = ws_o$Y, yrep = posterior_predict(fit_b1, draws = 1000), stat="prop_zero")


fit_b2 <- brm(bf(Y|trials(C) ~ 1,  zi ~ Region), data = ws_o1, family = zero_inflated_binomial(), chains = 2)
summary(fit_b2)
conditional_effects(fit_b2)
fit_b2
(prop_zero_test2 <- pp_check(fit_b2, plotfun = "stat", stat = "prop_zero"))
yrepnzb <- posterior_predict(fit_b2)
(prop_zero_test4 <- ppc_stat(y=ws_o1$Y, yrepnzb, stat=function(y) mean(y==0)))

fit_b3 <- brm(bf(Y|trials(C) ~ Region), data = ws_o1, family = zero_inflated_binomial(), chains = 2)
summary(fit_b3)
plot(conditional_effects(fit_b3), ask = FALSE)
yrepnzb <- posterior_predict(fit_b3)
(prop_zero_test4 <- ppc_stat(y=ws_o1$Y, yrepnzb, stat=function(y) mean(y==0)))

fit_b4 <- brm(bf(Y|trials(C) ~ Month + Region), data = ws_o, family = zero_inflated_binomial(), chains = 2)
summary(fit_b4)
plot(conditional_effects(fit_b4), ask = FALSE)
yrepnzb <- posterior_predict(fit_b4)
(prop_zero_test4 <- ppc_stat(y=ws_o1$Y, yrepnzb, stat=function(y) mean(y==0)))
(max_test_nb <- pp_check(fit_b4, plotfun = "stat", stat = "max"))

tls_predictions1 <- predict(fit_b1, newdata = ws_o)

plot(ws_o$Y, tls_predictions1[,1], pch = 16, main = "mod 1", ylim = c(0, 50))
plot(ws_o$Y, tls_predictions2[,1], pch = 16, main = "mod 2", ylim = c(0, 50))
plot(ws_o$Y, tls_predictions3[,1], pch = 16, main = "mod 3", ylim = c(0, 100))
plot(ws_o$Y, tls_predictions4[,1], pch = 16, main = "mod 4", ylim = c(0, 100))

prop_zero <- function(y) mean(y == 0)
(prop_zero_test1 <- pp_check(fit_b1, plotfun = "stat", stat = "prop_zero"))

# compare both models
fit_b1 <- add_criterion(fit_b1, "waic")
fit_b4 <- add_criterion(fit_b4, "waic")
loo_compare(fit_b1, fit_b4, criterion = "waic") # best fitting model appears on top (value == 0)
model_weights(fit_b1, fit_b4, weights = "loo") # higher value best model, divide by others and get how many times better


fit_b2 <- brm(Y|trials(C) ~ Month + Region,  data = ws_o1, family = zero_inflated_binomial(), chains = 2)
summary(fit_b2)
marginal_effects(fit_b2)
(max_test_znb <- ppc_stat(y=roaches$y, yrepnzb, stat="max"))
## spatial autocorrelation with moran's I
# https://rspatial.org/raster/analysis/3-spauto.html

## remove 25% of data for validation