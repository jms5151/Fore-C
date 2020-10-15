# Function to estimate thermal tolerance landscape from static assays
# General procedure
# Step 1: Calculate CTmax and z from TDT curve
# Step 2: Calculate average log10 time and Ta (mean x and y for interpolation purposes)
# Step 3: Interpolating survival probabilities to make them comparable across treatments 
# Step 4: Overlap all survival curves into a single one by shifting each curve to mean x and y employing z
# Step 5: Build expected survival curve with mean x and y pooling all data
# Step 6: Expand expected curve to multiple Ta (with 0.1ÂºC difference for predictive purposes)

strain_temp4 <- read.csv("Vcor/data/strain_temp4.csv")

ta <- strain_temp4$Tank_temperature
time <- strain_temp4$Time_to_infection


tolerance.landscape <- function(ta,time){
  # ta = temperature
  # time = days
  data <- data.frame(ta, time)
  data <- data[order(data$ta, data$time),]
  
  # Step 1: Calculate CTmax and z from TDT curve
  ta <- as.numeric(levels(as.factor(data$ta)))			
  model <- lm(log10(data$time) ~ data$ta)
  summary(model)
  ctmax <- -coef(model)[1]/coef(model)[2]
  z <- -1/coef(model)[2]
  
  # Step 2: Calculate average log10 time and Ta (mean x and y for interpolation purposes)
  time.mn <- mean(log10(data$time))
  ta.mn <- mean(data$ta)
  
  # Step 3: Interpolating survival probabilities to make them comparable across treatments 
  # breaking the time into 1000 bins between 100% survival and 0% survival, 
  # the length (# bins) differs by temperature
  time.interpol <- matrix(, 1001, length(ta))
  for(i in 1:length(ta)){	
    time <- c(0,sort(data$time[data$ta == ta[i]])) # time 0 to max time to all infected/dead, equivalent
    p <- seq(0, 100, length.out = length(time)) # divide 0 - 100% survival by number of observations, equivalent probability bins
    time.interpol[,i] <- approx(p, time, n = 1001)$y # empirical survival
    }			
  
  # Step 4: Overlap all survival curves into a single one by shifting each curve to mean x and y employing z
  # Step 5: Build expected survival curve with median survival time for each survival probability
  # scaling step size by sensitivity?
  shift <- (10^((ta - ta.mn)/z)) 
  time.interpol.shift <- t(t(time.interpol)*shift)[-1,] # shift curves to be on top of each other at mean temp, remove first row of zeros
  surv.pred <- 10^apply(log10(time.interpol.shift), 1, median) # calculates the median time to infection
  # given each time increment (e.g., median of each row in time.interpol matrix) 
  
  # Step 6: Expand predicted survival curves to measured Ta (matrix m arranged from lower to higher ta)
  # Step 7: Obtain predicted values comparable to each empirical measurement
  m <- surv.pred * matrix((10^((ta.mn - rep(ta, each = 1000))/z)), nrow = 1000) # matrix is deviation from median temperature divided by sensitivity
  out <-0
  for(i in 1:length(ta)){
    time <- c(0,data$time[data$ta == ta[i]])
    p <- seq(0, 100, length.out = length(time))
    out <- c(out, approx(seq(0, 100, length.out = 1000), m[,i], xout = p[-1])$y) # predicted time to infection given temperature deviation from median temp curve
    }
  data$time.pred <- out[-1]
  colnames(m) <- paste("time.at", ta,sep = ".")
  m <- cbind(surv.prob = seq(1, 0.001, -0.001), m)

  # plot(-10,-10,las=1,xlab="Time (min)",ylab="Survival (%)",col="white",xaxs="i",yaxs="i",xlim=c(0,max(data$time)*1.05),ylim=c(0,105))
  # par(mfrow=c(1,2),mar=c(4.5,4,1,1),cex.axis=1.1)
  par(mfrow=c(2, 3),mar=c(4.5, 4, 1, 1), cex.axis = 1.1)
  for(i in 1:length(ta)){
    plot(-10,-10,las=1,xlab="Time (days)",ylab="Survival (%)",col="white",xaxs="i",yaxs="i",xlim=c(0,max(data$time)*1.05),ylim=c(0,105))
    time <- c(0, sort(data$time[data$ta == ta[i]]))
    p <- seq(100, 0, length.out = length(time))
    points(time, p, pch=21, bg="black")
    time <- c(0, sort(data$time.pred[data$ta == ta[i]]))
    points(m[,i+1], 100*m[,1], type="l", lty=2)
    legend("topright", paste0("Temperature = ", ta[i]), bty='n')
    }
  # segments(max(data$time)*0.7,90,max(data$time)*0.8,90,lty=2)
  # text(max(data$time)*0.82,90,"fitted",adj=c(0,0.5))
  plot(log10(data$time.pred), log10(data$time), pch=21, bg="black", cex=0.5, lwd=0.7, las=1, xlab="Fitted Log10 time", ylab="Measured Log10 time")
  abline(lm(log10(data$time) ~ log10(data$time.pred)), col = 'red') # regression line
  abline(0, 1, lty=2) # unity line
  rsq <- round(summary(lm(log10(data$time) ~ log10(data$time.pred)))$r.square,3)
  text(min(log10(data$time.pred)), max(log10(data$time)), substitute("r"^2*" = "*rsq),adj=c(0,1))
  text(min(log10(data$time.pred)), max(log10(data$time)), paste0("CTMax = ", round(as.numeric(ctmax),2)),adj=c(0,4))
  text(min(log10(data$time.pred)), max(log10(data$time)), paste0("z = ", round(as.numeric(z),2)),adj=c(0,6))  
  list(ctmax = as.numeric(ctmax), 
       z = as.numeric(z), 
       ta.mn = ta.mn,  
       S = data.frame(surv = seq(0.999, 0, -0.001), time = surv.pred),
       time.obs.pred = cbind(data$time, data$time.pred), rsq = rsq)
  }					

t2 <- tolerance.landscape(strain_temp4$Tank_temperature, strain_temp4$Time_to_infection)

# Function to estimate survival probability from tolerance landscapes and environmental temperature data
dynamic.landscape <- function(ta,tolerance.landscape){
  surv <- t2$S[,2]
  ta.mn <- t2$ta.mn
  z <- t2$z
  shift <- 10^((ta.mn - ta)/z)	
  time.rel <- 0
  alive <- 100
  for(i in 1:length(ta)){			
    if(alive[length(na.omit(alive))] > 0) {							
      alive <- try(c(alive,approx(c(0,shift[i]*surv),
                                  seq(100,0, length.out = length(c(0,surv))), # 100 = number of time steps, seems arbitrary
                                  xout = time.rel[i] + 1)$y),
                   silent=TRUE)		
      time.rel <- try(c(time.rel,
                        approx(seq(100,0,length.out = length(c(0,surv))),
                               c(0,shift[i + 1]*surv),
                               xout = alive[i + 1])$y)
                      ,silent=TRUE)
      }
    else{
      alive <- 0}}				
  out <- data.frame(cbind(ta=ta[1:(length(alive)-1)],time=(1:length(ta))[1:(length(alive)-1)],alive=alive[1:(length(alive)-1)]))
  par(mar=c(4,4,1,1),mfrow=c(1,2))
  plot(1:length(ta),ta,type="l",xlim=c(0,length(ta)),ylim=c(min(ta),max(ta)),col="black",lwd=1.5,las=1,
       xlab = "Time (days)", ylab = "Temperature (ºC)")			
  plot(out$time,out$alive,type="l",xlim=c(0,length(ta)),ylim=c(0,100),col="black",lwd=1.5,las=1,
       xlab = "Time (days)", ylab = "Susceptible (%)")
  list(time = out$time,ta = out$ta, alive = out$alive)}

jamie.dynamic.landscape <- function(ta, tolerance.landscape){
  surv <- tolerance.landscape$S[,2]
  ta.mn <- tolerance.landscape$ta.mn
  z <- tolerance.landscape$z
  shift <- 10^((ta.mn - ta)/z)	# added negative sign to delta T
  susceptible <- 0
  for(i in 1:length(ta)){
    apporox_suscept_day1 <- approx(c(0, shift[i]*surv),
                                   seq(100, 0, length.out = length(c(0, surv))),
                                   xout = 1)$y
    # plot(c(0, shift[i]*surv), seq(100, 0, length.out = length(c(0, surv))))
    susceptible <- c(susceptible, 100-apporox_suscept_day1)
    }				
  out <- data.frame(cbind(ta=ta[1:(length(susceptible)-1)],time=(1:length(ta))[1:(length(susceptible)-1)],susceptible=susceptible[1:(length(susceptible)-1)]))
  par(mar=c(4,4,1,1),mfrow=c(1,3))
  plot(1:length(ta),ta,type="l",xlim=c(0,length(ta)),ylim=c(min(ta),max(ta)),col="black",lwd=1.5,las=1,
       xlab = "Time (days)", ylab = "Temperature (ºC)")			
  plot(out$time,out$susceptible,type="l",xlim=c(0,length(ta)),ylim=c(0,100),col="black",lwd=1.5,las=1,
       xlab = "Time (days)", ylab = "Susceptible (%)")
  list(time = out$time,ta = out$ta, susceptible = out$susceptible)}

# test new function
load("Vcor/resp_fun/TempData_Silbiger.RData")
temp_p <- subset(temp, Region == "Panama")
temp_b <- subset(temp, Region == "Bermuda")
dl <- jamie.dynamic.landscape(temp_p$Temp, t2)

library(zoo)

thermal_accum_fun <- function(suscept_ts){
  suscept_time_series <- suscept_ts[[3]]
  my_fun = function(x) { w = length(x):1; sum(x*exp(-x*w))} # FIXME 
  rollapply(suscept_time_series, width=100, by=1, fill=NA, FUN=my_fun, by.column=FALSE, align="right")
}

taf <- thermal_accum_fun(dl)
plot.ts(taf, ylab = "Accumulated susceptibility")
