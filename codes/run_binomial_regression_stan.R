rm(list=ls()) #remove previous variable assignments

# load library
library(rstan)
library(shinystan)
library(boot)

options(mc.cores=4)

# simulate data with multiple predictor variables -----------
N <- 100 # number of sites
C <- round(runif(N, min=10, max=100)) # number of colonies on a transect
temp <- scale(rnorm(N,27,1)) # driver variable 1: temperature values at each transect
chl <- scale(rnbinom(N, 0.14, 0.08)) # driver variable 2: chl-a values at each transect
dev <- scale(rnbinom(N,0.18, 0.05)) # driver variable 3: development index (night time lights) at each transect
X <- t(data.frame(temp, chl, dev))
b0 <- -3 # intercept
b1 <- 1 # slope (increase in log odds of disease per unit x)
b2 <- 1
b3 <- 1
mu <- (X[1,]*b1 + X[2,]*b2 + X[3,]*b3 + b0) # generating function to describe logit prevalence
p <- inv.logit(mu) # prevalence 
M <- 3 # dimensions of x (# driver vars)
Y <- rbinom(N,C,p) # number of diseased colonies on each transect

# plot simulated data -----
# plot(X[1,],p, pch=16, xlab="Temperature", ylab="Prevalence")
# title("simulated data")
# plot(X,mu, pch=16, xlab="Temperature")
# plot(p,Y, pch=16, xlab="Prevalence", ylab="# diseased colonies")
# plot(X,Y, pch=16, xlab="Temperature", ylab="# diseased colonies")

# new data to generate -----
N_new <- 1000
temp_new <- seq(from=min(X[1,]), to=max(X[1,]), length=N_new)
chl_new <- seq(from=min(X[2,]), to=max(X[2,]), length=N_new)
dev_new <- seq(from=min(X[3,]), to=max(X[3,]), length=N_new)
X_new <- t(data.frame(temp_new, chl_new, dev_new))

# compile model
binomMod <- stan_model('codes/binomial_model.stan')

# pass data to stan and run model
fit2 <- sampling(binomMod, list(N=N, C=C, X=X, M=M, Y=Y, N_new=N_new, X_new=X_new), iter=1000, chains=4)

# assess model fit
launch_shinystan(fit2)

# Output ---------------------------------------------------------------------
# extract data
list_of_draws <- extract(fit2)
ga <- data.frame(cbind(colMeans(list_of_draws$Y_ppc), list_of_draws$beta[1], list_of_draws$beta[2], list_of_draws$beta[3], list_of_draws$b0, colMeans(list_of_draws$theta)))
colnames(ga) <- c("Y_ppc", "b1", "b2", "b3", "b0", "theta")

# plot
plot(X[1,], inv.logit(median(ga$b0) + median(ga$b1)*X[1,] + median(ga$b2)*X[2,] + median(ga$b3)*X[3,]), pch=16, xlab='Temperature', ylab='mu')
plot(X[2,], inv.logit(median(ga$b0) + median(ga$b1)*X[1,] + median(ga$b2)*X[2,] + median(ga$b3)*X[3,]), pch=16, xlab='Chl-a', ylab='mu')
plot(X[3,], inv.logit(median(ga$b0) + median(ga$b1)*X[1,] + median(ga$b2)*X[2,] + median(ga$b3)*X[3,]), pch=16, xlab='Development', ylab='mu')
