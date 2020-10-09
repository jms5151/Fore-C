data { 
  int N_gamma;                                 
  vector[N_gamma] T_gamma;                     
  vector[N_gamma] gamma; 
  int N_new;
  vector[N_new] T_new;
} 

parameters { 
  real<lower=0, upper=20> shape_gamma;
  real<lower=0, upper=10> scale_gamma;
  real constant_gamma;
  real<lower=0,upper=22> T0_gamma;
  real<lower=0> sigma_gamma;
} 

model { 
  shape_gamma ~ uniform(0,20);
  scale_gamma ~ uniform(0,10);
  sigma_gamma ~ inv_gamma(0.1, 0.1);
  
  // gamma: recovery rate, which varies with temperature
  for(k in 1:N_gamma){
    real mu_gamma = constant_gamma * 
    ((1/(tgamma(shape_gamma)*
    pow(scale_gamma, shape_gamma)))*
    pow((T_gamma[k] - T0_gamma), (shape_gamma-1))*
    exp(-(T_gamma[k] - T0_gamma)/scale_gamma)); 
    gamma[k] ~ normal(mu_gamma, sigma_gamma);
    
  // this is only really needed if we have data below the expected min.  
  if (T0_gamma > T_gamma[k]) {
     target += positive_infinity();
   }

  }
}

generated quantities {

  real gamma_ppc[N_gamma];
  vector[N_new] gamma_new;

  for (o in 1:N_gamma) {
    real gamma_ppc_mu = constant_gamma *
    ((1/(tgamma(shape_gamma)*
    scale_gamma^shape_gamma))*
    pow((T_gamma[o] - T0_gamma), (shape_gamma-1))*
    exp(-(T_gamma[o] - T0_gamma)/scale_gamma));
    gamma_ppc[o] = normal_rng(gamma_ppc_mu, sigma_gamma);
  }

  for (w in 1:N_new){
    gamma_new[w]  = constant_gamma *
    ((1/(tgamma(shape_gamma)*
    scale_gamma^shape_gamma))*
    pow((T_new[w] - T0_gamma), (shape_gamma-1))*
    exp(-(T_new[w] - T0_gamma)/scale_gamma));
    
  if (T0_gamma > T_new[w]){
      gamma_new[w] = 0.001;
    }
    
  }
}
