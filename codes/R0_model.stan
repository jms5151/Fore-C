data { 
  int N_gr;                                 
  vector[N_gr] T_gr;                     
  vector[N_gr] GR;                         
  int N_ip;                                 
  vector[N_ip] T_ip;                     
  vector[N_ip] IP;
  int N_r;                                 
  vector[N_r] T_r;                     
  vector[N_r] Recovery;                         
  int N_new;
  vector[N_new] T_new;
} 

parameters { 
  real<lower=0, upper=1> constant_gr;
  real<lower=0,upper=24> T0_gr;
  real<lower=24,upper=100> Tm_gr;
  real<lower=0> sigma_gr;
  real<lower=0, upper=1> constant_ip;
  real<lower=0,upper=30> T0_ip;
  real<lower=31,upper=100> Tm_ip;
  real<lower=0> sigma_ip;
  real alpha_r; // Intercept
  real beta_r; // Slope (regression coefficients)
  real<lower=0> sigma_r; 
} 

model { 
  // priors
  constant_gr ~ uniform(0,1); 
  T0_gr ~ uniform(0,24);
  Tm_gr ~ uniform(25,100);
  sigma_gr ~ uniform(0,1000); 
  constant_ip ~ uniform(0,1); 
  T0_ip ~ uniform(0,24);
  Tm_ip ~ uniform(25,100);
  sigma_ip ~ uniform(0,1000); 

  // growth rate model
  for(i in 1:N_gr){                  
    real mu_gr = constant_gr * T_gr[i] * (T_gr[i] - T0_gr) * sqrt(Tm_gr - T_gr[i]);
    GR[i] ~ normal(mu_gr, sigma_gr);
  }
  if (T0_gr > Tm_gr) {
    target += positive_infinity();
  }
  // infection probability model
  for(j in 1:N_ip){                  
    real mu_ip = constant_ip * T_ip[j] * (T_ip[j] - T0_ip) * sqrt(Tm_ip - T_ip[j]);
    IP[j] ~ normal(mu_ip, sigma_ip);
  }
  if (T0_ip > Tm_ip) {
    target += positive_infinity();
  }
  // recovery rate model
  Recovery ~ normal(T_r * beta_r + alpha_r, sigma_r);
}

generated quantities {
  
  real GR_ppc[N_gr];
  real IP_ppc[N_ip];
  real Recovery_ppc[N_r];
  vector[N_new] R0;
  
  for (k in 1:N_gr){
    real mu_ppc_gr = constant_gr * T_gr[k] * (T_gr[k] - T0_gr) * sqrt(Tm_gr - T_gr[k]);
    GR_ppc[k] = normal_rng(mu_ppc_gr, sigma_gr);
  }
  
  for (l in 1:N_ip){
    // if ((T_ip[l] > T0_ip) && (T_ip[l] < Tm_ip)){
    real mu_ppc_ip = constant_ip * T_ip[l] * (T_ip[l] - T0_ip) * sqrt(Tm_ip - T_ip[l]);
    IP_ppc[l] = normal_rng(mu_ppc_ip, sigma_ip);      
    // } 
  }
  
  for (m in 1:N_r) {
    Recovery_ppc[m] = normal_rng(T_r[m] * beta_r + alpha_r, sigma_r);
  }

  for (w in 1:N_new){
    R0[w] =
    (normal_rng(constant_gr * T_new[w] * (T_new[w] - T0_gr) * sqrt(Tm_gr - T_new[w]), sigma_gr) *
    normal_rng(constant_ip * T_new[w] * (T_new[w] - T0_ip) * sqrt(Tm_ip - T_new[w]), sigma_ip)) /
    normal_rng(T_new[w] * beta_r + alpha_r, sigma_r);
  }
}
