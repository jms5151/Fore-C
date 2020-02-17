# load libraries
library(rstan)
library(shinystan)
library(boot)

<<<<<<< HEAD
# Briere ---------------------------------------------------------
# simulate data
temp <- seq(11, 36, 1)
LHT <- rnorm(length(temp), (0.007 * temp * (temp - 10) * sqrt(37 - temp)), 1) 
# plot(temp, LHT, pch=16)
N <- length(temp)
tempNew <- seq(22, 33, 0.1) # temperature gradient to calculate derived quantities over
N_new <-length(tempNew)

# use real data
# load data
# gr <- read.csv("C:/Users/Jeremy/Desktop/Vcor_papers/data/Kushmaro_etal_1998_vibrio_ak1_growth_rate.csv", head=T, stringsAsFactors=F)
=======
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
>>>>>>> 38d17bcbaba615bdb4ee55b28a8a3c6e056a4132
# N <- nrow(gr)
# temp <- gr$Temperature
# LHT <- gr$Growth_rate_vibrio_ak1

<<<<<<< HEAD
# compile model
briere_traitModel <- stan_model('codes/Briere_trait_fitting.stan')

# pass data to stan and run model
init_fn <- function() {
  list(constant=0.00005, T0=8, Tm=45, sigma=0.03)
}
briereFit <- sampling(briere_traitModel, list(N=N, temp=temp, LHT=LHT, N_new=N_new, tempNew=tempNew), init=init_fn, iter=1000, chains=4) #, tempNew=tempNew, N_new=N_new
summary(briereFit)
# fit2 <- sampling(traitMod, list(N=N, temp=temp, LHT=LHT), iter=1000, chains=4) #, tempNew=tempNew, N_new=N_new
# we want to save fit2 if possible
list_of_draws <- extract(briereFit)
print(names(list_of_draws))

# assess model fit
launch_shinystan(briereFit)

# Quadratic ---------------------------------------------------------
# simulate data
temp <- seq(11, 36, 1)
LHT <- rnorm(length(temp), (-1 * 0.07 * (temp - 10) * (temp - 37)), 1) 
plot(temp, LHT, pch=16)
N <- length(temp)

# compile model
quadratic_traitModel <- stan_model('codes/Quadratic_trait_fitting.stan')

# pass data to stan and run model
init_fn <- function() {
  list(constant=0.05, T0=8, Tm=40, sigma=0.03)
}
quad.fit <- sampling(quadratic_traitModel, list(N=N, temp=temp, LHT=LHT), init=init_fn, iter=1000, chains=4) #, tempNew=tempNew, N_new=N_new
summary(quad.fit)

# assess model fit
launch_shinystan(quad.fit)

# linear regression ----------------------------------------------------------------
# simulate data
temp <- seq(11, 36, 1)
LHT <- rnorm(length(temp), (temp * -0.3 + 0.1), 1) 
LHT <- LHT + abs(min(LHT))
# plot(temp, LHT, pch=16)
N <- length(temp)
tempNew <- seq(5, 45, 0.1) # temperature gradient to calculate derived quantities over
N_new <-length(tempNew)

# compile model
linear_traitModel <- stan_model('codes/linear_regression.stan')

# pass data to stan and run model
linearFit <- sampling(linear_traitModel, list(N=N, temp=temp, LHT=LHT, N_new=N_new, tempNew=tempNew), iter=1000, chains=4) #, tempNew=tempNew, N_new=N_new

# other data -----------------------------------------------------------------------
s1 <- read.csv("C:/Users/Jeremy/Desktop/Vcor_papers/data/Ushijima_etal_2014_Mcap_infected_with_vcor_survival.csv", head=T)
s2 <- read.csv("C:/Users/Jeremy/Desktop/Vcor_papers/data/Ushijima_etal_2016_Acyt_infected_with_vcor_survival.csv", head=T)
s3 <- read.csv("C:/Users/Jeremy/Desktop/Vcor_papers/data/Ushijima_etal_2016_Mcap_infected_with_vcor_survival.csv", head=T)

library(tidyverse)
survival <- rbind(s1, s2, s3) %>%
  group_by(Temperature, Coral_spp, Vcor_spp) %>%
  filter(Day == max(Day))
survival$Infection_probability <- 100-survival$Percent_survival 

infectious_period <- data.frame("Temperature"=c(23,27,29), "Days"=c(3:1))
=======
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
>>>>>>> 38d17bcbaba615bdb4ee55b28a8a3c6e056a4132
