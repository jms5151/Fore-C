# load libraries
library(rstan)
library(shinystan)
library(boot)

# load data
gr <- read.csv("C:/Users/Jeremy/Desktop/Vcor_papers/data/Kushmaro_etal_1998_vibrio_ak1_growth_rate.csv", head=T, stringsAsFactors=F)

# simulate data
temp <- seq(11, 36, 1)
LHT <- rnorm(length(temp), (0.007 * temp * (temp - 10) * sqrt(37 - temp)), 1) 
# plot(temp, LHT)
N <- length(temp)
# tempNew <- seq(5, 45, 0.1) # temperature gradient to calculate derived quantities over
# N_new <-length(tempNew)

# use real data
# N <- nrow(gr)
# temp <- gr$Temperature
# LHT <- gr$Growth_rate_vibrio_ak1

# combine data into dataframe
# df <- data.frame(N=N, temp=temp, GR=GR, tempNew=tempNew, N_new=N_new) 

# compile model
traitMod <- stan_model('codes/Briere_trait_fitting.stan')

# pass data to stan and run model
fit2 <- sampling(traitMod, list(N=N, temp=temp, LHT=LHT), iter=1000, chains=4) #, tempNew=tempNew, N_new=N_new
# we want to save fit2 if possible

# assess model fit
launch_shinystan(fit2)


# // generated quantities {
#   //     for(i in 1:N_new){
#     //     real mu_new[i] = -1 * constant * (tempNew[i] - T0) * (tempNew[i] - Tm) * (Tm > tempNew[i]) * (T0 < tempNew[i])
#     //     LHT_new[i] ~ normal(mu_new[i], sigma);
#     //     }
#   // }
