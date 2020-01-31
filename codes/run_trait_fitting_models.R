# load libraries
library(rstan)
library(shinystan)
library(boot)

# load data
gr <- read.csv("C:/Users/Jeremy/Desktop/Vcor_papers/data/Kushmaro_etal_1998_vibrio_ak1_growth_rate.csv", head=T, stringsAsFactors=F)

# set data
N <- nrow(gr)
temp <- gr$Temperature
GR <- gr$Growth_rate_vibrio_ak1
# tempNew <- seq(5, 45, 0.1) # temperature gradient to calculate derived quantities over
# N_new <-length(tempNew)

# combine data into dataframe
# df <- data.frame(N=N, temp=temp, GR=GR, tempNew=tempNew, N_new=N_new) 

# compile model
traitMod <- stan_model('codes/Briere_trait_fitting.stan')

# pass data to stan and run model
fit2 <- sampling(binomMod, list(N=N, temp=temp, GR=GR, tempNew=tempNew, N_new=N_new), iter=1000, chains=4)
# we want to save fit2 if possible

# assess model fit
launch_shinystan(fit2)
