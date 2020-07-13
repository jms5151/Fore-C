data { 
  int N_gamma;                                 
  vector[N_gamma] T_gamma;                     
  vector[N_gamma] gamma; 
  int N_new;
  vector[N_new] T_new;
} 

parameters { 
  real<lower=0> shape_gamma;
  real<lower=0> scale_gamma;
  real<upper=100> constant_gamma;
  real<lower=0,upper=22> T0_gamma;
  real<lower=0> sigma_gamma;
} 

model { 
  // gamma: recovery rate, which varies with temperature
  for(k in 1:N_gamma){
    real mu_gamma = constant_gamma * 
    ((1/(tgamma(shape_gamma)*
    pow(scale_gamma, shape_gamma)))*
    pow((T_gamma[k] - T0_gamma), (shape_gamma-1))*
    exp(-(T_gamma[k] - T0_gamma)/scale_gamma)); 
    gamma[k] ~ normal(mu_gamma, sigma_gamma);
  }
}

generated quantities {

  real gamma_ppc[N_gamma];
  vector[N_new] gamma_new;

  for (o in 1:N_gamma) {
    gamma_ppc[o] = constant_gamma *
    ((1/(tgamma(shape_gamma)*
    scale_gamma^shape_gamma))*
    pow((T_gamma[o] - T0_gamma), (shape_gamma-1))*
    exp(-(T_gamma[o] - T0_gamma)/scale_gamma));
  }
  
  for (w in 1:N_new){
    gamma_new[w]  = constant_gamma *
    ((1/(tgamma(shape_gamma)*
    scale_gamma^shape_gamma))*
    pow((T_gamma[w] - T0_gamma), (shape_gamma-1))*
    exp(-(T_gamma[w] - T0_gamma)/scale_gamma));
  }
}
