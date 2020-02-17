data { 
  int N;                                 // number of observations of traits
  vector[N] temp;                        // vector of temperatures
  vector[N] LHT;                         // vector of trait (e.g., growth rate)
  // int N_new;                          // number of new temperatures
  // vector tempNew[N_new];              // vector of new temperatures
} 

parameters { 
  real<lower=0, upper=1> constant;
  real<lower=0> T0;
  real<lower=0> Tm;
  real<lower=0> sigma;
} 

model { 
  // priors
  constant ~ uniform(0,1); //gamma(1,1)
  T0 ~ uniform(0,24);
  Tm ~ uniform(25,100);
  sigma ~ uniform(0,1000); //inv_gamma(0.1,0.1)

  for(i in 1:N){                  
    real mu = -1 * constant * (temp[i] - T0) * (temp[i] - Tm);
    LHT[i] ~ normal(mu, sigma);
  }
  if (T0 > Tm) {
    target += positive_infinity();
  }
}
