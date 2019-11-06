# 
library(brms)
options(mc.cores = parallel::detectCores())

# simulate data
t <- 100 # number of transects
x <- scale(rnorm(t,27,2)) # driver variable 1: temperature values at each transect
N <- round(runif(t, min=10, max=100)) # number of colonies on a transect
prev0 <- 0.05 # baseline prevalence
b0 <- log(prev0/(1-prev0)) # logit of prev0 - intercept with prevalence of 5%  
b1 <- 0.2 # slope (increase in log odds of disease per unit x)
p <- exp(b0+b1*x)/(1+exp(b0+b1*x)) # inverse logit to calculate probability of disease as a function of temperature 
y <- rbinom(t,N,p) # number of diseased colonies on each transect

# compile data into dataframe
df <- data.frame(y=y, N=N, x=x)

# visualize simulated data
plot(x, y, cex=N/max(N), pch=16) # plot diseased colonies vs temperature with point size scaled by number of colonies on transect
hist(N)
plot(x, p, pch=16)

# binomial regression model with logit link
s <- stanvar(scode = "vector[N] p_hat = inv_logit(temp_Intercept + Xc * b);", block = "genquant") # create a generated quantity, here we are using prevalence

mod <- brm(y | trials(N) ~ x, family=binomial(link=logit)
           , stanvars = s
           , data=df, cores=4)

stancode(mod)

summary(mod)
marginal_effects(mod)
launch_shinystan(mod)

# predict
yrep <- posterior_predict(mod, df) # random samples from posterior distribution
