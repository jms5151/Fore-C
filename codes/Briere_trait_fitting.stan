data { 
  int N;                                 // number of observations of traits
  vector[N] temp;                        // vector of temperatures
  vector[N] LHT;                         // vector of trait (e.g., growth rate)
<<<<<<< HEAD
  int N_new;                          // number of new temperatures
  vector[N_new] tempNew;              // vector of new temperatures
=======
  // int N_new;                          // number of new temperatures
  // vector tempNew;                     // vector of new temperatures
>>>>>>> 38d17bcbaba615bdb4ee55b28a8a3c6e056a4132
} 

parameters { 
  real<lower=0, upper=1> constant;
<<<<<<< HEAD
  real<lower=0,upper=24> T0;
  real<lower=24,upper=100> Tm;
=======
  real<lower=0> T0;
  real<lower=0> Tm;
>>>>>>> 38d17bcbaba615bdb4ee55b28a8a3c6e056a4132
  real<lower=0> sigma;
} 

model { 
  // priors
<<<<<<< HEAD
    constant ~ uniform(0,1); //gamma(1,1)
    T0 ~ uniform(0,24);
    Tm ~ uniform(25,100);
    sigma ~ uniform(0,1000); //inv_gamma(0.1,0.1)
    
  // model; https://mc-stan.org/docs/2_21/functions-reference/logical-functions.html
  for(i in 1:N){                  // Briere equation: c*x*(x-T0)*sqrt(Tm-x)
    real mu = constant * temp[i] * (temp[i] - T0) * sqrt(Tm - temp[i]);
    LHT[i] ~ normal(mu, sigma);
  }
  if (T0 > Tm) { 
    target += positive_infinity();
  }
}

generated quantities {
  // posterior predictive checks
  real LHT_ppc[N];
  vector[N_new] LHT_new;
  
  for (n in 1:N){
    real mu_ppc = constant * temp[n] * (temp[n] - T0) * sqrt(Tm - temp[n]);
    LHT_ppc[n] = normal_rng(mu_ppc, sigma);
  }
  
  // predict on new data
  for (w in 1:N_new){
    // if ((tempNew[w] > T0) && (tempNew[w] < Tm)){
    real mu_new = constant * tempNew[w] * (tempNew[w] - T0) * sqrt(Tm - tempNew[w]);
    LHT_new[w] = normal_rng(mu_new, sigma);
    // }
  }
=======
    constant ~ uniform(0,1);
    T0 ~ uniform(1,24);
    Tm ~ uniform(25,45);
    sigma ~ inv_gamma(0.1,0.1); //uniform(0,1000)
    
  // model; https://mc-stan.org/docs/2_21/functions-reference/logical-functions.html
  for(i in 1:N){                  // Briere equation: c*x*(x-T0)*sqrt(Tm-x)
    real mu = constant * temp[i] * (temp[i] - T0) * sqrt((Tm - temp[i]) * (Tm < temp[i])) * (T0 > temp[i]);
    LHT[i] ~ normal(mu, sigma);
  }
>>>>>>> 38d17bcbaba615bdb4ee55b28a8a3c6e056a4132
}
