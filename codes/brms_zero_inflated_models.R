# ZIB
# https://avehtari.github.io/modelselection/roaches.html
# https://bookdown.org/ajkurz/DBDA_recoded/model-comparison-and-hierarchical-modeling.htmlhttps://bookdown.org/ajkurz/DBDA_recoded/model-comparison-and-hierarchical-modeling.html


# model diagnostics
# https://easystats.github.io/performance/index.html
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

# loo_compare(fit_b1, fit_b4, criterion = "waic") # best fitting model appears on top (value == 0)

# Model comparisons  -----------------------------------------------------------------------------
devtools::session_info()
# load libraries
library(brms)
library(bayesplot)
# rstanarm zero inflated models
# https://github.com/stan-dev/rstanarm/blob/master/vignettes/count.Rmd
# library(rstanarm)
# library(bayesplot)

# options(mc.cores=6)

# load data
load("Compiled_data/GA_with_predictors.RData")
load("Compiled_data/WS_with_predictors.RData")

# GA Pacific -----------------------------------------------
ga_pac <- subset(GA_data, Region != "GBR" & Family == "Poritidae")
ga_pac <- ga_pac[, c("Y", 
                     "C", 
                     "Median_colony_size", 
                     "BlackMarble_2016_3km_geo.1",
                     "SST_90dMean",
                     "Winter_condition",
                     "H_abund",
                     "Poritidae_mean_cover",
                     "Long_Term_Chl_Variability"
                     )] 
ga_pac <- ga_pac[complete.cases(ga_pac), ]

# set priors 
# get_prior(bf(Y|trials(C) ~ scale(Median_colony_size) +
#                scale(BlackMarble_2016_3km_geo.1) +
#                scale(SST_90dMean) +
#                scale(Winter_condition) +
#                scale(H_abund) +
#                scale(Poritidae_mean_cover) +
#                scale(Long_Term_Chl_Variability)
#              ),
#           data = ga_pac,
#           family = zero_inflated_binomial())
# 
# ga_pac_priors <- c(prior(dnorm( 0 , 1.5 ), class = "b", coef = "scaleMedian_colony_size"),
#                    prior(dnorm( 0 , 1.5 ), class = "b", coef = "scaleBlackMarble_2016_3km_geo.1"))


# run model
GA_Pacific_model <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                             scale(BlackMarble_2016_3km_geo.1) +
                             scale(SST_90dMean) +
                             scale(Winter_condition) + 
                             scale(H_abund) +
                             scale(Poritidae_mean_cover) +
                             scale(Long_Term_Chl_Variability)
                           ),
                             data = ga_pac,
                             family = zero_inflated_binomial(),
                             chains = 3 #, prior = ga_pac_priors
                        )

conditional_effects(GA_Pacific_model)
summary(GA_Pacific_model)

saveRDS(GA_Pacific_model, file = "Compiled_data/model_objects/GA_Pacific_model_binomial.Rds")

# GA_Pacific_model <- readRDS("Compiled_data/model_objects/GA_Pacific_model.Rds")

# zero-inflated poisson model (doesn't make much of a difference) 
GA_Pacific_model2 <- brm(Y ~ scale(Median_colony_size) +
                             scale(BlackMarble_2016_3km_geo.1) +
                             scale(SST_90dMean) +
                             scale(Winter_condition) +
                             scale(H_abund) +
                             scale(Poritidae_mean_cover) +
                             scale(Long_Term_Chl_Variability),
                         data = ga_pac,
                         family = zero_inflated_poisson(),
                         chains = 3
                         )

model_weights(GA_Pacific_model,
              GA_Pacific_model2,
              weights = "loo")

saveRDS(GA_Pacific_model2, file = "Compiled_data/model_objects/GA_Pacific_model.Rds")
# stan code
# make_stancode(formula = (bf(Y|trials(C) ~ scale(Median_colony_size) +
#                     scale(BlackMarble_2016_3km_geo.1) +
#                     scale(SST_90dMean) +
#                     scale(Winter_condition) +
#                     scale(H_abund) +
#                     scale(Poritidae_mean_cover) +
#                     scale(Long_Term_Chl_Variability)
#                     )),
#               data = ga_pac,
#               family = zero_inflated_binomial())

# predict
ga_pac_predictions <- predict(GA_Pacific_model, 
                              newdata = ga_pac, 
                              summary = FALSE)

# calculate prevalence and re-organize into data frame 
# with each column representing ID 1 - N, and each row
# a sample from the posterior distribution
prev_predictions <- t(t(ga_pac_predictions)/ga_pac$C)
prev_predictions <- as.data.frame(prev_predictions)
prev_predictions <- sapply( 1:nrow(ga_pac) , function(k) mean(k) )
plot(ga_pac$Y, prev_predictions)

# checks
plot(GA_Pacific_model)

ga_pac_yrep <- posterior_predict(GA_Pacific_model)

ga_pac_prop_zero <- ppc_stat(y = ga_pac$Y, 
                             ga_pac_yrep, 
                             stat = function(y) mean(y==0)
                             )


ga_pac_max_znb <- ppc_stat(y = ga_pac$Y, 
                           ga_pac_yrep, 
                           stat = "max")

ga_pac_max_znb

ga_pac_loop <- loo(GA_Pacific_model)
plot(ga_pac_loop)

pp_check(GA_Pacific_model)

pp_check(GA_Pacific_model, 
         type = "bars", 
         nsamples = 100)

ga_pac_prop_zero <- function(x) {sum(x == 0)/length(x)}

ppc_stat(y = ga_pac$Y, 
         yrep = posterior_predict(GA_Pacific_model, draws = 1000), 
         stat = "prop_zero")

# library(rethinking)
# ga_dat <- list(
#   Y = ga_pac$Y,
#   C = ga_pac$C,
#   Median_colony_size = scale(ga_pac$Median_colony_size),
#   development = scale(ga_pac$BlackMarble_2016_3km_geo.1)
# )
# 
# gapor_ulam <- ulam(
#   alist(
#     Y ~ dbinom( C , p ) ,
#     logit(p) <- beta_size * Median_colony_size + beta_dev * development,
#     beta_size ~ dnorm( 0 , 1.5 ),
#     beta_dev ~ dnorm( 0 , 1.5 )
#   ) , data=ga_dat , chains=4 , log_lik=TRUE )
# 
# precis( gapor_ulam , depth=2 )
# stancode(gapor_ulam)
# prior <- extract.prior( gapor_ulam , n=1e4 )
# dens(inv_logit(prior$beta_size))
# dens(inv_logit(prior$beta_dev))
# 
# post <- extract.samples(gapor_ulam)
# plot(precis(post))
# postcheck( gapor_ulam ) # blue = observations, black = predictions (median, 89%iles)
# pairs(gapor_ulam) # posterior correlations among parameters

# WS Pacific Acropora -------------------------------------
ws_pac_acr <- subset(WS_data, Region != "GBR" & !is.na(Y) & Family == "Acroporidae")
ws_pac_acr <- ws_pac_acr[, c("Y", 
                             "C", 
                             "wave_sd",
                             # "Region",
                             "Three_Week_Kd_Median",                             
                             "Three_Week_Median_Residual",
                             "Acute_Kd_12week",
                             "Long_Term_Chl_Median",
                             "H_abund",
                             "Hot_snaps"
                             )]
ws_pac_acr <- ws_pac_acr[complete.cases(ws_pac_acr), ]

WS_Pacific_Acropora_model <- brm(bf(Y|trials(C) ~ scale(wave_sd) +
                                      # Region +
                                      scale(Three_Week_Kd_Median) +
                                      scale(Three_Week_Median_Residual) +
                                      scale(Acute_Kd_12week) +
                                      scale(Long_Term_Chl_Median) +
                                      scale(H_abund) +
                                      scale(Hot_snaps)
                                    ),
                                 data = ws_pac_acr,
                                 family = zero_inflated_binomial(),
                                 chains = 3)

conditional_effects(WS_Pacific_Acropora_model)
summary(WS_Pacific_Acropora_model)

saveRDS(WS_Pacific_Acropora_model, file = "Compiled_data/model_objects/WS_Pacific_Acropora_model_binomial.Rds")

# WS_Pacific_Acropora_model <- readRDS("Compiled_data/model_objects/WS_Pacific_Acropora_model.Rds")

# zero-inflated poisson model
WS_Pacific_Acropora_model2 <- brm(Y ~ scale(wave_sd) +
                                      scale(Three_Week_Kd_Median) +
                                      scale(Three_Week_Median_Residual) +
                                      scale(Acute_Kd_12week) +
                                      scale(Long_Term_Chl_Median) +
                                      scale(H_abund) +
                                      scale(Hot_snaps),
                                  data = ws_pac_acr,
                                  family = zero_inflated_poisson(),
                                  chains = 3)
conditional_effects(WS_Pacific_Acropora_model2)
summary(WS_Pacific_Acropora_model2)
model_weights(WS_Pacific_Acropora_model,
        WS_Pacific_Acropora_model2,
        weights = "loo")

saveRDS(WS_Pacific_Acropora_model2, file = "Compiled_data/model_objects/WS_Pacific_Acropora_model.Rds")

# predict
ws_pac_predictions <- predict(WS_Pacific_Acropora_model, 
                              newdata = ws_pac_acr, 
                              summary = FALSE)

# calculate prevalence and re-organize into data frame 
# with each column representing ID 1 - N, and each row
# a sample from the posterior distribution
prev_predictions <- t(t(ws_pac_predictions)/ws_pac_acr$C)
prev_predictions <- as.data.frame(prev_predictions)
prev_predictions <- sapply( 1:nrow(ws_pac_acr) , function(k) mean(k) )
plot(ws_pac_acr$Y, prev_predictions)

# alternative approach
plot(WS_Pacific_Acropora_model)

ws_pac_acr_yrep <- posterior_predict(WS_Pacific_Acropora_model)

ws_pac_acr_prop_zero <- ppc_stat(y = ws_pac_acr$Y,
                                 ws_pac_acr_yrep,
                                 stat = function(y) mean(y==0)
                                 )

ws_pac_acr_max <- ppc_stat(y = ws_pac_acr$Y, 
                           ws_pac_acr_yrep, 
                           stat = "max")
ws_pac_acr_max

ws_pac_acr_loop <- loo(WS_Pacific_Acropora_model)
plot(ws_pac_acr_loop)

pp_check(WS_Pacific_Acropora_model)

pp_check(WS_Pacific_Acropora_model, 
         type = "bars", 
         nsamples = 100)

ws_pac_acr_prop_zero <- function(x) {sum(x == 0)/length(x)}

ppc_stat(y = ws_pac_acr$Y, 
         yrep = posterior_predict(WS_Pacific_Acropora_model, draws = 1000), 
         stat = "prop_zero")
# https://www.monicaalexander.com/posts/2020-28-02-bayes_viz/

# GA GBR ---------------------------------------------------
ga_gbr <- subset(GA_data, Region == "GBR")
ga_gbr <- ga_gbr[, c("Y",
                     "Winter_condition",
                     "Hot_snaps",
                     "Acute_Kd_12week",
                     "Island",
                     "Long_Term_Kd_Variability",
                     "wave_sd",
                     "Acute_Chla_4week",
                     "SST_90dMean")]

ga_gbr <- ga_gbr[complete.cases(ga_gbr), ]
ga_gbr$Y <- as.integer(ga_gbr$Y)

GA_GBR_model <- brm(bf(Y ~ scale(Winter_condition) +
                         scale(Hot_snaps) +
                         scale(Acute_Kd_12week) +
                         Island +
                         scale(Long_Term_Kd_Variability) +
                         scale(wave_sd) +
                         scale(Acute_Chla_4week) + 
                         scale(SST_90dMean)),
              family = zero_inflated_poisson(),
              data = ga_gbr,
              chains = 2) 

conditional_effects(GA_GBR_model)
summary(GA_GBR_model)

saveRDS(GA_GBR_model, file = "Compiled_data/model_objects/GA_GBR_model.Rds")

# poisson priors
# mean outcome = exp(mean + variance^2/2)
# curve( dlnorm( x , 3 , 0.5 ) , from=0 , to=100 , n=200 )                        

# predict
ga_gbr_predictions <- predict(GA_GBR_model, 
                              newdata = ga_gbr, 
                              summary = FALSE)

# calculate prevalence and re-organize into data frame 
# with each column representing ID 1 - N, and each row
# a sample from the posterior distribution
prev_predictions <- t(t(ga_gbr_predictions))
prev_predictions <- as.data.frame(prev_predictions)
prev_predictions <- sapply( 1:nrow(ga_gbr) , function(k) mean(k) )
plot(ga_gbr$Y, prev_predictions)

# WS GBR ---------------------------------------------------
ws_gbr <- subset(WS_data, Region == "GBR")
ws_gbr <- ws_gbr[, c("Y", 
                     "Fish_abund", 
                     "Winter_condition", 
                     "Month", 
                     "wave_sd", 
                     "Hot_snaps", 
                     "Coral_cover",
                     "Three_Week_Chl_Variability",
                     "Island",
                     "Acute_Chla_8week" #12week pretty similar
                     )]

ws_gbr <- ws_gbr[complete.cases(ws_gbr), ]
ws_gbr$Y <- as.integer(ws_gbr$Y)

WS_GBR_model <- brm(bf(Y ~ scale(Fish_abund) +
                         scale(Winter_condition) +
                         Month +
                         scale(wave_sd) + 
                         scale(Hot_snaps) +
                         scale(Coral_cover) +
                         scale(Three_Week_Chl_Variability) +
                         Island +
                         scale(Acute_Chla_8week)),
                    family = zero_inflated_poisson(),
                    data = ws_gbr,
                    chains = 3) 

conditional_effects(WS_GBR_model)
summary(WS_GBR_model)

saveRDS(WS_GBR_model, file = "Compiled_data/model_objects/WS_GBR_model.Rds")

# predict
ws_gbr_predictions <- predict(WS_GBR_model, 
                              newdata = ws_gbr, 
                              summary = FALSE)

# calculate prevalence and re-organize into data frame 
# with each column representing ID 1 - N, and each row
# a sample from the posterior distribution
prev_predictions <- t(t(ws_gbr_predictions))
prev_predictions <- as.data.frame(prev_predictions)
prev_predictions <- sapply( 1:nrow(ws_gbr) , function(k) mean(k) )
plot(ws_gbr$Y, prev_predictions)

# run with simulated data array ----------------------------
mod <- readRDS("Compiled_data/model_objects/gaPac_SizeFishWaveSDSSTMonthNight.Rds")

# Make data to predict on
medColSize <- seq(0, 200, 50) #seq(min(ga_pac$Median_colony_size), max(ga_pac$Median_colony_size), by = 50)
FishAbundance <- seq(0.01, 1., 0.3)#seq(0.01, round(max(ga_pac$H_abund)), by = 0.3) # 0.1
Lights <- seq(0, 255, 50)#seq(0, 255, by = 50)
waveSD <- seq(0, 42, 20)#seq(min(ga_pac$wave_sd), max(ga_pac$wave_sd), by = 20)
SST <- seq(20, 30, 2)#seq(round(min(ga_pac$SST_90dMean)-3), round(max(ga_pac$SST_90dMean)), by = 2)
months <- seq(1, 12, 2)#seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-31"), by = "2 months")
#months <- format(months, "%m")

# create array
Lcol <- length(medColSize)
Lfish <- length(FishAbundance)
Llights <- length(Lights)
LwaveSD <- length(waveSD)
Lsst <- length(SST)
Lmonths <- length(months)

Nunique <- Lcol * Lfish * Llights * LwaveSD * Lsst * Lmonths
ids <- 0:Nunique
# ids <- paste0("V", ids)

newDataArray <- array(data = ids,
                      dim = c(Lcol, Lfish, Llights, LwaveSD, Lsst, Lmonths),
                      dimnames = list("Median_colony_size" = medColSize,
                                      "H_abund" = FishAbundance,
                                      "BlackMarble_2016_3km_geo.1" = Lights,
                                      "wave_sd" = waveSD,
                                      "SST_90dMean" = SST,
                                      "Month" = months
                                      )
                      )

# save array for Gang
save(newDataArray, file = "Compiled_data/Array_for_Gang.Rds")
load(file = "Compiled_data/Array_for_Gang.Rds")

# format as long dataframe
newdata <- as.data.frame.table(newDataArray)
newdata[,1:5] <- lapply(newdata[,1:5], as.character)
newdata[,1:5] <- lapply(newdata[,1:5], as.numeric)
newdata$Month <- as.character(newdata$Month)

# add simulated number of colonies
C  <- rnbinom(n = nrow(newdata)*1.5, size = 2, mu = mean(ga_pac$C))
C <- C[C > 5]
C <- sample(C, nrow(newdata))
newdata$C <- C

# predict
ga_predictions <- predict(mod, newdata = newdata, summary = F)

# calculate prevalence 
prev_predictions <- t(t(ga_predictions)/newdata$C)

# save data for Gang
outputGang <- as.data.frame(prev_predictions)
outputGang <- outputGang[1:250,]
write.csv(outputGang, "Compiled_data/posterior_predictions_Gang_small.csv", row.names = F)

# ----------------
nestedTest <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) #+
                                  # (1|Region) +
                                  # (1|Month)
),
data = ga_pac,
family = zero_inflated_binomial(),
# mc.cores = parallel::detectCores(),
chains = 1)

conditional_effects(nestedTest)
summary(nestedTest)


gaPac_SizeCovWaveMean <- brm(bf(Y|trials(C) ~ scale(Median_colony_size) +
                              scale(Poritidae_mean_cover) +
                              # scale(H_abund) +
                              scale(wave_mean) +
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