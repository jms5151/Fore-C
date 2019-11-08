# load library
library(rstan)
library(shinystan)
library(boot)

# simulate data
N <- 100 # number of sites
C <- round(runif(N, min=10, max=100)) # number of colonies on a transect
X <- t(scale(rnorm(N,27,2))) # driver variable 1: temperature values at each transect
b0 <- -3 # intercept
b1 <- 0.2 # slope (increase in log odds of disease per unit x)
sigma <- 0.2
mu <- rnorm(N, (X * b1 + b0), sigma)
p <- inv.logit(mu) # inverse logit (exp(mu)/(1+exp(mu))) to calculate probability of disease as a function of temperature 
M <- 1 # dimensions of x (# driver vars)
Y <- rbinom(N,C,p)
# plot(p,Y, pch=16)
# plot(x,Y, pch=16)
df <- data.frame(N=N, C=C, X=X, M=M, Y=Y)

# compile model
binomMod <- stan_model('codes/binomial_model.stan')

# pass data to stan and run model
options(mc.cores=4)
fit2 <- sampling(binomMod, list(N=N, C=C, X=X, M=M, Y=Y), iter=1000, chains=4)

# diagnose
print(fit2)

# plot
params <- extract(fit2)
hist(params$theta)
hist(params$lambda) # mean(params$lambda) should be approximately = mean(y[y>0])
launch_shinystan(fit2)