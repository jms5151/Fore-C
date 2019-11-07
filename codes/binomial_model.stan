data { 
  int N;                              // number of sites
  vector[N] C;                        // number of colonies per site
  vector[N] Y;                        // number of diseased colonies per site
  int M;                              // number of predictor variables
  matrix[M,N] X;                      // number of predictor variables by site
  // int N_new;                          // number of new sites
  // vector[N_new] C_new;                // number of new colonies per site
  // matrix[M,N_new] X_new;              // number of predictors variables by new sites
} 

parameters { 
  vector[M] beta;                     // vector of predictor variable coefficients
  real b0;                            // intercept
  vector<lower=0, upper=1> p;         // prevalence
  real<lower=0> sig_p;                // standard deviation of prevalence
} 

model { 
  for (m in 1:M){
    beta[m] ~ normal(0,10);           // priors for predictor variable coefficients
  }

  sig_p ~ normal(0,5);                // prior for standard deviation of prevalence

  for (n in 1:N){
    real mu = X[,n]' * beta + b0;     // mean prevalence is a function of predictor variables and intercept, redefined for each site
    logit(p[n]) ~ normal(mu, sig_p);  // prevalence is normally distributed with mean and standard deviation
    Y[n] ~ binomial(C[n], p[n]);      // the number of diseased colonies is binomially distributed given the number of colonies and prevalence probability
  }
} 

generated quantities { 
  // posterior predictive checks
  vector[N] mu_ppc;                   // vector of mean prevalence values by site 
  vector[N] p_ppc;                    // vector of realizations of prevalence values by site 
  vector[N] Y_ppc;                    // vector of number of diseased colonies per site 

  for (n in 1:N){
    real mu = X[,n]' * beta + b0;                   // mean prevalence is a function of predictor variables and intercept, redefined for each site
    logit(p_ppc[n]) ~ normal_rng(mu_ppc[n], sig_p); // prevalence is normally distributed with mean and standard deviation
    // Y_ppc[n] ~ binomial_rng(C[n], p_ppc[n]);        // the number of diseased colonies is binomially distributed given the number of colonies and prevalence probability
  }
  
  // predict on new data
  // vector[N_new] mu_new;               // vector of new mean prevalence values by site
  // vector[N_new] p_new;                // vector of new realizations of prevalence values by site
  // vector[N_new] Y_new;                // vector of new number of diseaed colonies per site
  // 
  //  for (w in 1:N_new){
  //   real mu_new = X[,w]' * beta + b0;               // mean prevalence is a function of predictor variables and intercept, redefined for each site
  //   logit(p_new[w]) ~ normal_rng(mu_new[w], sig_p); // prevalence is normally distributed with mean and standard deviation
  //   Y_new[w] ~ binomial_rng(C_new[w], p_new[w]);    // the number of diseased colonies is binomially distributed given the number of colonies and prevalence probability
  // }
}

