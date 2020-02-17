# load library
library(rstan)
library(shinystan)
library(boot)

options(mc.cores=4)

# load real data ----------
load("merged_data_tls.R")
N <- nrow(tls)
C <- tls$C
Y <- tls$Y
X <- tls$HotSnap
M <- 1 # dimensions of x (# driver vars)

# p <- tls$p
# par(mfrow=c(2,2))
# plot(X,p, pch=16, xlab="Temperature", ylab="Prevalence")
# title("actual data")
# plot(X,mu, pch=16, xlab="Temperature")
# plot(p,Y, pch=16, xlab="Prevalence", ylab="# diseased colonies")
# plot(X,Y, pch=16, xlab="Temperature", ylab="# diseased colonies")

# simulate data -----------
N <- 100 # number of sites
C <- round(runif(N, min=10, max=100)) # number of colonies on a transect
X <- t(scale(rnorm(N,27,2))) # driver variable 1: temperature values at each transect
b0 <- -3 # intercept
b1 <- 1 # slope (increase in log odds of disease per unit x)
mu <- (X * b1 + b0) # generating function to describe logit prevalence
p <- inv.logit(mu) # prevalence 
M <- 1 # dimensions of x (# driver vars)
Y <- rbinom(N,C,p) # number of diseased colonies on each transect
# plot simulated data -----
# plot(X,p, pch=16, xlab="Temperature", ylab="Prevalence")
# title("simulated data")
# plot(X,mu, pch=16, xlab="Temperature")
# plot(p,Y, pch=16, xlab="Prevalence", ylab="# diseased colonies")
# plot(X,Y, pch=16, xlab="Temperature", ylab="# diseased colonies")

# new data to generate -----
N_new <- 1000
X_new <- t(seq(from=min(X), to=max(X), length=N_new))

# combine data into dataframe
df <- data.frame(N=N, C=C, X=X, M=M, Y=Y, N_new=N_new, X_new=X_new) 

# compile model
binomMod <- stan_model('codes/binomial_model.stan')

# pass data to stan and run model
fit2 <- sampling(binomMod, list(N=N, C=C, X=X, M=M, Y=Y, N_new=N_new, X_new=X_new), iter=1000, chains=4)
# we want to save fit2 if possible

# assess model fit
launch_shinystan(fit2)
