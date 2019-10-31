# create basic hurdle model with simulated data
library(brms)
options(mc.cores = parallel::detectCores())

# simulate data
PA <- rbinom(1000, 1, 0.1) # simulate presence/absence data
N <- length(PA) 
y <- round(rgamma(N,2,2))*PA # simulate severity data with left skewed distribution (beta distributed)
g1 <- sort(rep(1:25,N/25)) # simulate 25 sites
x1 <- rbeta(N,2,4)*100 # simulate colony size data
x1 <- ifelse(PA==0, x1-10, x1+10) # make sizes more extreme between healthy and diseased colonies
x2 <- rnorm(length(unique(g1)),27,2) # simulate temperature data
x2 <- sort(rep(x2,N/length(unique(g1))))
x2 <- ifelse(x2 > mean(x2[y==0]), x2+1.5, x2-1.5) # make temperature more extreme between healthy and diseased colonies
df <- data.frame(health_state = y, site = g1, size = x1, temp = x2)

# Gamma
fit_hurdGam <- brm(bf(health_state ~ size, hu ~ size), data = df, family = hurdle_gamma())

summary(fit_hurdGam)
marginal_effects(fit_hurdGam)
launch_shinystan(fit_hurdGam)
# pp_check(fit_hurdLN2, type = "xyz") # get an overview of all valid types
pp_check(fit_hurdGam)  # shows dens_overlay plot by default
pp_check(fit_hurdGam, type = "scatter_avg", nsamples = 100)
pp_check(fit_hurdGam, type = "stat_2d")

# predict
forecast <- posterior_predict(fit_hurdGam, df)
plot(forecast)
mean(forecast)
