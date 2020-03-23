# load libraries
library(rstan)
library(shinystan)
library(boot)
library(rstan)
library(matrixStats)

# Data -----------------------------------------------------------------------
# simulate data
temp <- c(rep(15,2), rep(20,5), rep(25,8), rep(30,4), rep(33,3)) # temp <- seq(11, 36, 1)
LHT <- rnorm(length(temp), (temp * -0.3 + 11), 1) 
plot(temp, LHT, pch=16)
N <- length(temp)
tempNew <- seq(5, 45, 0.1) # temperature gradient to calculate derived quantities over
N_new <-length(tempNew)

# Model ----------------------------------------------------------------------
# compile model
linear_traitModel <- stan_model('codes/linear_regression.stan')

# pass data to stan and run model
linearFit <- sampling(linear_traitModel, list(N=N, temp=temp, LHT=LHT, N_new=N_new, tempNew=tempNew), iter=1000, chains=4) #, tempNew=tempNew, N_new=N_new

# Output ---------------------------------------------------------------------
# extract data
list_of_draws <- extract(linearFit)

# plot recovery rate (infectious period) ------------
RecoverySamples <- data.frame(list_of_draws$LHT_new)
RecoveryMeans<-colMeans(RecoverySamples)
RecoveryQuantiles<-colQuantiles(list_of_draws$LHT_new)

# plot
plot(tempNew, RecoveryMeans, type='l', lwd=2, ylab='Infectious period (days)', xlab=expression(paste("Temperature (",degree,"C)")))
lines(tempNew, RecoveryQuantiles[,2], lty=2, col='red')
lines(tempNew, RecoveryQuantiles[,4], lty=2, col='red')
points(temp, LHT, pch=16)
