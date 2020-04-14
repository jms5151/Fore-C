rm(list=ls()) #remove previous variable assignments

# load library
library(rstan)
library(shinystan)
library(boot)
library(ggplot2)
library(rstanarm)

options(mc.cores=4)

# simulate data -----------
N <- 100 # number of sites
C <- round(runif(N, min=10, max=100)) # number of colonies on a transect
temp <- scale(rnorm(N,27,1)) # driver variable 1: temperature values at each transect
chl <- c(scale(log(rgamma(N/2, 0.14, 0.08))), scale(log(rgamma(N/2, 5, 0.02)))) ## driver variable 2: chl-a values at each transect
dev <- scale(rnbinom(N,0.18, 0.05)) # driver variable 3: development index (night time lights) at each transect
X <- t(data.frame(temp, chl, dev))
b0 <- -3 # intercept
b1 <- 1 # slope (increase in log odds of disease per unit x)
b2 <- 1
b3 <- 1
mu <- (X[1,]*b1 + X[2,]*b2 + X[3,]*b3 + b0) # generating function to describe logit prevalence
p <- inv.logit(mu) # prevalence 
M <- nrow(X) # dimensions of x (# driver vars)
Y <- rbinom(N,C,p) # number of diseased colonies on each transect

N_new <- 1000
temp_new <- seq(from=min(X[1,]), to=max(X[1,]), length=N_new)
chl_new <- seq(from=min(X[2,]), to=max(X[2,]), length=N_new)
dev_new <- seq(from=min(X[3,]), to=max(X[3,]), length=N_new)
X_new <- t(data.frame(temp_new, chl_new, dev_new))

# real data -------------
load("Compiled_data/GA_w_predictor_data.RData")
N <- nrow(merged_ga)
C <- merged_ga$C
temp <- scale(merged_ga$SST_MMM) # attr(temp,"scaled:center"), attr(temp,"scaled:scale")
chl <- scale(merged_ga$CHL_MMM)
dev <- scale(merged_ga$NightLights)
X <- t(data.frame(temp, chl, dev))
M <- nrow(X)
Y <- merged_ga$Y

load("Compiled_data/SST_MMM.RData")
load("Compiled_data/CHL_MMM.RData")
load("Compiled_data/Night_Lights.RData")

temp_new <- seq(from=min(SST_MMM$SST_MMM, na.rm=T), to=max(SST_MMM$SST_MMM, na.rm=T), by=0.5)
temp_new <- scale(temp_new, center = attr(temp,"scaled:center"), scale = attr(temp,"scaled:scale"))
chl_new <- seq(from=min(CHL_MMM$CHL_MMM, na.rm=T), to=max(CHL_MMM$CHL_MMM, na.rm=T), by=1)
chl_new <- scale(chl_new, center = attr(chl,"scaled:center"), scale = attr(chl,"scaled:scale"))
dev_new <- seq(from=min(nightLights_df$BlackMarble_2016_3km_geo, na.rm=T), to=max(nightLights_df$BlackMarble_2016_3km_geo, na.rm=T), by=1)
dev_new <- scale(dev_new, center = attr(dev,"scaled:center"), scale = attr(dev,"scaled:scale"))
X_new <- expand.grid(temp_new, chl_new, dev_new, KEEP.OUT.ATTRS = FALSE)
colnames(X_new) <- c("temp", "chl", "dev")
X_new <- data.frame(t(X_new))
N_new <- ncol(X_new)

X_new_backtransformed <- as.data.frame(t(X_new))
X_new_backtransformed$temp_new <- X_new_backtransformed$temp_new * attr(temp, 'scaled:scale') + attr(temp, 'scaled:center')
X_new_backtransformed$chl_new <- X_new_backtransformed$chl_new * attr(chl, 'scaled:scale') + attr(chl, 'scaled:center')
X_new_backtransformed$dev_new <- X_new_backtransformed$dev_new * attr(dev, 'scaled:scale') + attr(dev, 'scaled:center')
X_new_backtransformed$index <- seq(1,nrow(X_new_backtransformed),1)
X_new_backtransformed$index <- paste0("V", X_new_backtransformed$index)
save(X_new_backtransformed, file="Compiled_data/ga_condition_index.RData")

# compile model
binomMod <- stan_model('codes/binomial_model.stan')

# pass data to stan and run model
fit2 <- sampling(binomMod, list(N=N, C=C, X=X, M=M, Y=Y, N_new=N_new, X_new=X_new), iter=1000, chains=4)

# assess model fit
launch_shinystan(fit2)

# Output ---------------------------------------------------------------------
# https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html
# extract data
list_of_draws <- extract(fit2)
Y_ppc_vals <- colMeans(list_of_draws$Y_ppc)
# betas & b0 are vectors (with length equal to the number of post-warmup iterations times the number of chains)
# theta will be a matrix, with each column corresponding to one of N number of sites
ga <- data.frame(cbind(list_of_draws$beta[,1], list_of_draws$beta[,2], list_of_draws$beta[,3], list_of_draws$b0))
colnames(ga) <- c("b1", "b2", "b3", "b0") 
mu <- as.data.frame(list_of_draws$theta)

# save prevalence data
# prevalence <- as.data.frame(colMeans(list_of_draws$theta_new))
# colnames(prevalence) <- "Prevalence" 
# prevalence$index <- seq(1,nrow(prevalence),1)
# prevalence$index <- paste0("V", prevalence$index)
# save(prevalence, file="Compiled_data/prevalence_samples.RData")

prevalence <- as.data.frame(list_of_draws$theta_new)
colnames(prevalence) <- seq(1,ncol(prevalence),1) 
colnames(prevalence) <- paste0("V", colnames(prevalence)) 
prevalence <- prevalence %>% gather()
colnames(prevalence) <- c("index", "Prevalence")
save(prevalence, file="Compiled_data/prevalence_samples.RData")

# plots
plot(Y_ppc_vals, Y, pch=16, xlab='Predicted number of diseased colonies (Y_ppc)', ylab="Observed number of diseased colonies (Y)")

plot(X[1,], colMeans(mu), pch=16, xlab='MMM SST', ylab='mu')
plot(X[2,], colMeans(mu), pch=16, xlab='MMM Chl-a', ylab='mu', xlim=c(-1,5))
plot(X[3,], colMeans(mu), pch=16, xlab='Development (Night lights)', ylab='mu')
plot(merged_ga$p, colMeans(mu), pch=16, xlab='observed prevalence', ylab='mu')

# stan_plot(fit2, pars=c("b0", "beta[1]", "beta[2]", "beta[3]"))
stan_plot(fit2, pars=c("beta[1]", "beta[2]", "beta[3]")) + 
  geom_vline(xintercept = 0, color = "black", size=1.5)


