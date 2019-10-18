# hurdle models 
# https://mc-stan.org/docs/2_20/stan-users-guide/zero-inflated-section.html

# load library
library(rstan)
library(shinystan)

# simulate data
PA <- rbinom(500, 1, 0.1) # simulate presence/absence data
N <- length(PA) # simulate presence/absence data
y <- round(rpois(N,6))*PA # simulate severity data with left skewed distribution

# compile model
model2 <- stan_model('codes/basic_hurdle_model.stan')

# pass data to stan and run model
options(mc.cores=4)
fit2 <- sampling(model2, list(N=N, y=y), iter=1000, chains=4)

# diagnose
print(fit2)

# plot
params <- extract(fit2)
hist(params$theta)
hist(params$lambda) # mean(params$lambda) should be approximately = mean(y[y>0])
launch_shinystan(fit2)
