data { 
  int N;                                 // number of observations of traits
  vector[N] temp;                        // vector of temperatures
  vector[N] LHT;                         // vector of trait (e.g., growth rate)
  // int N_new;                          // number of new temperatures
  // vector tempNew;                     // vector of new temperatures
} 

parameters { 
  real<lower=0, upper=1> constant;
  real<lower=0> T0;
  real<lower=0> Tm;
  real<lower=0> sigma;
} 

model { 
  // priors
    constant ~ uniform(0,1);
    T0 ~ uniform(1,24);
    Tm ~ uniform(25,45);
    sigma ~ inv_gamma(0.1,0.1); //uniform(0,1000)
    
  // model; https://mc-stan.org/docs/2_21/functions-reference/logical-functions.html
  for(i in 1:N){                  // Briere equation: c*x*(x-T0)*sqrt(Tm-x)
    real mu = constant * temp[i] * (temp[i] - T0) * sqrt((Tm - temp[i]) * (Tm < temp[i])) * (T0 > temp[i]);
    LHT[i] ~ normal(mu, sigma);
  }
}
