data { 
  int N;                                 // number of observations of traits
  vector[N] temp;                        // vector of temperatures
  vector[N] LHT;                         // vector of trait (e.g., growth rate)
  // int N_new;                          // number of new temperatures
  // vector tempNew;                     // vector of new temperatures
} 

parameters { 
  real constant;
  real T0;
  real Tm;
  real sigma;
} 

model { 
  // priors
    constant ~ uniform(0,1);
    T0 ~ uniform(0,24);
    Tm ~ uniform(25,45);
    sigma ~ gamma(2,0);
  }  
  // model; https://mc-stan.org/docs/2_21/functions-reference/logical-functions.html
  for(i in 1:N){                  // Briere equation: c*x*(x-T0)*sqrt(Tm-x)
    real mu[i] = constant * temp[i] * (temp[i] - T0) * sqrt((Tm - temp[i]) * int operator>(real Tm, real temp[i])) * int operator<(real T0, real temp[i]);
    LHT[i] ~ normal(mu[i], sigma);
    }
}

// generated quantities {
//     for(i in 1:N_new){
//     real mu_new[i] = -1 * constant * (tempNew[i] - T0) * (tempNew[i] - Tm) * (Tm > tempNew[i]) * (T0 < tempNew[i])
//     LHT_new[i] ~ normal(mu_new[i], sigma);
//     }
// }