library(rstan)
library(bayesplot)
library(matrixStats)

# load and format data
recovery <- read.csv("Vcor/data/gamma_temp.csv")
recovery <- recovery[order(recovery$Tank_temperature),]

stan_gamma_model_data <-
  list(
    N_gamma = nrow(recovery),
    T_gamma = recovery$Tank_temperature, 
    gamma = recovery$Time_to_infection,
    T_new = seq(21,33,0.01),
    N_new = length(seq(21,33,0.01))
  )

stan_gamma_model_fit <- sampling(
  stan_model("codes/gamma.stan"),
  data = stan_gamma_model_data,
  iter = 1000
)

# save and open stanfit object
saveRDS(stan_gamma_model_fit, "mod.rds")
stan_gamma_model_fit <- readRDS("mod.rds")

# plots
rstan::traceplot(stan_gamma_model_fit, par = c('lp__', "shape_gamma","scale_gamma","constant_gamma"), ncol = 2)
bayesplot::mcmc_acf(as.matrix(stan_gamma_model_fit), pars = c("shape_gamma","scale_gamma","constant_gamma"))
bayesplot::mcmc_areas(as.matrix(stan_gamma_model_fit), pars = c("shape_gamma","scale_gamma","constant_gamma"), prob = 0.95)

# extract data
list_of_draws <- rstan::extract(stan_gamma_model_fit)
# print(names(list_of_draws))

GammaSamples <- data.frame(list_of_draws$gamma_new)
GammaMeans<-colMeans(GammaSamples)
GammaQuantiles<-colQuantiles(list_of_draws$gamma_new, probs = c(0.05, 0.95))

# plot model vs data
plot(stan_gamma_model_data[[4]], GammaMeans, type='l', lwd=2, ylab='Daily removal rate', xlab=expression(paste("Temperature (",degree,"C)")), ylim=c(0,40))
lines(stan_gamma_model_data[[4]], GammaQuantiles[,1], lty=2, col='red', ylim=c(0,40))
lines(stan_gamma_model_data[[4]], GammaQuantiles[,2], lty=2, col='red', ylim=c(0,40))
points(stan_gamma_model_data[[2]], stan_gamma_model_data[[3]], pch=16, ylim=c(0,40))

