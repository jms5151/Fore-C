data {
  int N; // Sample size
  vector[N] temp; // Predictor
  vector[N] LHT; // Outcome
  int N_new;                          // number of new temperatures
  vector[N_new] tempNew;              // vector of new temperatures

}

parameters {
  real alpha; // Intercept
  real beta; // Slope (regression coefficients)
  real < lower = 0 > sigma; // Error SD
}

model {
  LHT ~ normal(temp * beta + alpha, sigma);
}

generated quantities {
  real LHT_rep[N];
  vector[N_new] LHT_new;
  
  for (n in 1:N) {
    LHT_rep[n] = normal_rng(temp[n] * beta + alpha, sigma);
  }
  
  for (w in 1:N_new){
    LHT_new[w] = normal_rng(tempNew[w] * beta + alpha, sigma);
  }
}
