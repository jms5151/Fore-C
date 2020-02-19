# load libraries
library(rstan)
library(shinystan)
library(boot)

# simulate data ----------------------------------------------------------------
# simulate growth rate data
T_gr <- seq(11, 36, 1)
GR <- rnorm(length(T_gr), (0.03 * T_gr * (T_gr - 10) * sqrt(37 - T_gr)), 4)
GR[GR<0] <- 0
N_gr <- length(T_gr)
plot(T_gr, GR, pch=16)

# simulate infection probability data
T_ip <- seq(20, 37, 1)
IP <- rnorm(length(T_ip), (0.07 * T_ip * (T_ip - 23) * sqrt(37 - T_ip)), 5) 
IP <- IP/max(IP)
N_ip <- length(T_ip)
plot(T_ip, IP, pch=16)

# simulate recovery rate (infectious period) data
T_r <- seq(11, 35, 1)
# T_r <- seq(13, 32, 2)
Recovery <- rnorm(length(T_r), (T_r * -0.3 + 11), 1) 
# Recovery <- Recovery + abs(min(Recovery))
N_r <- length(T_r)
plot(T_r, Recovery, pch=16)

# generated temperatures
T_new <- as.array(seq(15, 33, 0.1)) # temperature gradient to calculate derived quantities over
N_new <-length(T_new)

# real data -------------------------------------------------------------------
# growth rate data
gr.df <- read.csv("Vcor/data/Kushmaro_etal_1998_vibrio_ak1_growth_rate.csv", head=T, stringsAsFactors=F)
# N_gr <- nrow(gr.df)
# T_gr <- gr.df$Temperature
# GR <- gr.df$Growth_rate_vibrio_ak1 * 24
N_gr <- nrow(gr.df) + 1
T_gr <- c(gr.df$Temperature, 37)
GR <- c(gr.df$Growth_rate_vibrio_ak1, 0.35)
# plot(T_gr, GR, pch=16)

# infection probability data
infectionProb.df <- read.csv("Vcor/data/InfectionProbability.csv", head=T, stringsAsFactors=F)
infectionProb.df <- infectionProb.df[order(infectionProb.df$Temperature),]
T_ip <- infectionProb.df$Temperature
IP <- infectionProb.df$Infection_probability
N_ip <- length(T_ip)
plot(T_ip, IP, pch=16)

# infectious period (recovery rate) data
recovery.df <- read.csv("Vcor/data/InfectiousPeriod.csv", head=T, stringsAsFactors=F)
T_r <- recovery.df$Temperature
Recovery <- recovery.df$Days - 10
# Recovery <- scale(Recovery)
N_r <- length(T_r)
plot(T_r, Recovery, pch=16)

# generated temperatures
load("sst_tls.RData")
T_new <- seq(min(sst$SST, na.rm=T)-3, max(sst$SST, na.rm=T)+7, 0.1)
N_new <-length(T_new)

# run model -------------------------------------------------------------------
# compile model
R0_mod <- stan_model('codes/R0_model.stan')

# pass data to stan and run model
init_fn <- function() {
  list(constant_gr=0.05, T0_gr=8, Tm_gr=45, sigma_gr=0.03,
       constant_ip=0.05, T0_ip=20, Tm_ip=45, sigma_ip=0.03)
}

R0_fit <- sampling(R0_mod, list(N_gr=N_gr, T_gr=T_gr, GR=GR, N_ip=N_ip, T_ip=T_ip, IP=IP
                   , N_r=N_r, T_r=T_r, Recovery=Recovery, N_new=N_new, T_new=T_new)
                   , init=init_fn, iter=1000, chains=4) 

# assess model fit
launch_shinystan(R0_fit)

# plot output ----------------------------------------------------------------
# load libraries
library(rstan)
library(matrixStats)

# extract data
list_of_draws <- extract(R0_fit)
# print(names(list_of_draws))

# plot growth rate ---------------------------------
GrowthRateSamples <- data.frame(list_of_draws$GR_ppc)
GrowthRateMeans<-colMeans(GrowthRateSamples)
GrowthRateQuantiles<-colQuantiles(list_of_draws$GR_ppc)

# plot
plot(T_gr, GrowthRateMeans, type='l', lwd=2, ylab='Growth rate (generations/hr)', xlab=expression(paste("Temperature (",degree,"C)")))
lines(T_gr, GrowthRateQuantiles[,2], lty=2, col='red')
lines(T_gr, GrowthRateQuantiles[,4], lty=2, col='red')
# lines(temp, alpha_beta_summary[4:29,4], lty=2, col='darkred')
# lines(temp, alpha_beta_summary[4:29,5], lty=2, col='darkred')
points(T_gr, GR, pch=16)

# plot infection probability----------------------
InfectProbSamples <- data.frame(list_of_draws$IP_ppc)
InfectProbMeans<-colMeans(InfectProbSamples)
InfectProbQuantiles<-colQuantiles(list_of_draws$IP_ppc)

# plot
plot(T_ip, InfectProbMeans, type='l', lwd=2, ylab='Infection probability', xlab=expression(paste("Temperature (",degree,"C)")))
lines(T_ip, InfectProbQuantiles[,2], lty=2, col='red')
lines(T_ip, InfectProbQuantiles[,4], lty=2, col='red')
points(T_ip, IP, pch=16)

# plot recovery rate (infectious period) ------------
RecoverySamples <- data.frame(list_of_draws$Recovery_ppc)
RecoveryMeans<-colMeans(RecoverySamples)
RecoveryQuantiles<-colQuantiles(list_of_draws$Recovery_ppc)
# RecoveryQuantiles_summary <- summary(R0_fit, probs = c(0.025, 0.975))$summary

# plot
plot(T_r, RecoveryMeans, type='l', lwd=2, ylab='Infectious period (days)', xlab=expression(paste("Temperature (",degree,"C)")))
lines(T_r, RecoveryQuantiles[,2], lty=2, col='red')
lines(T_r, RecoveryQuantiles[,4], lty=2, col='red')
points(T_r, Recovery, pch=16)

# plot R0 model ---------------------------------
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
R0Samples <- data.frame(list_of_draws$R0)
R0Means<-colMeans(R0Samples)
# R0Means<-range01(colMeans(R0Samples))
R0Quantiles<-colQuantiles(list_of_draws$R0)

# plot
source("C:/Users/Jamie/Box Sync/R_functions/mtexti.R")
sst$RegionColor <- "deeppink4"
sst$RegionColor[sst$Region=="MARIAN"] <- "deepskyblue"
sst$RegionColor[sst$Region=="PRIAs"] <- "darkslategray2"
sst$RegionColor[sst$Region=="SAMOA"] <- "darkviolet"

par(mar = c(5.1, 4.1, 4.1, 5.1))
plot(T_new, R0Means, type='l', lwd=2, ylab='Transmission suitability', xlab=expression(paste("Temperature (",degree,"C)")), yaxt='n', xlim=c(21,36))
lines(T_new, R0Quantiles[,2], lty=2, col='red')
lines(T_new, R0Quantiles[,4], lty=2, col='red')
axis(side=2, las=2, at=c(-0.6,-0.4,-0.2,0.0), labels=c(0.2,0.4,0.6,0.8))
par(new = T)
sst$jittered_p <- jitter(sst$p)
plot(round(sst$SST, 1), sst$jittered_p, col='black', bg=sst$RegionColor, pch=21, yaxt='n', xaxt='n', ylab='', xlab='', xlim=c(21,36), ylim=c(0,0.25))
axis(side=4, las=2)
mtexti(side=4, "Prevalence")
legend("topright", legend=c("Hawaii", "Marianas", "PRIAs", "Am. Samoa"),
       text.col = c("deeppink4", "deepskyblue", "darkslategray2", "darkviolet"),
       pch = c(21,21,21,21), pt.bg=c("deeppink4", "deepskyblue", "darkslategray2", "darkviolet"), 
       col=c("black", "black", "black", "black"),
       bty='n', cex=1.2)
