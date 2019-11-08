data { 
  int N;                              // number of sites
  int C[N];                           // number of colonies per site
  int Y[N];                           // number of diseased colonies per site
  int M;                              // number of predictor variables
  matrix[M,N] X;                      // number of predictor variables by site
  int N_new;                          // number of new sites (places in our hypervolume niche)
  matrix[M,N_new] X_new;              // number of predictors variables by new sites
} 

// transformed data {
//   int PrevObs[N] = operator/(Y,C);
// }

parameters { 
  vector[M] beta;                     // vector of predictor variable coefficients
  real b0;                            // intercept
} 

// transformed parameters{
//   real<lower=0, upper=1> p[N] = inv_logit(theta);
// }

model { 
  for (m in 1:M){
    beta[m] ~ normal(0,10);           // priors for predictor variable coefficients
  }

  for (n in 1:N){
    real mu = X[,n]' * beta + b0;     // generating function to describe logit number of diseased colonies (per site, as a function of predictor variables and intercept)
    Y[n] ~ binomial_logit(C[n], mu);  // number of diseased colonies
  }
} 

generated quantities {
  // posterior predictive checks
  int Y_ppc[N];                       // vector of number of diseased colonies per site
  real<lower=0, upper=1> theta[N];    // vector of realizations of prevalence values by site
  real<lower=0, upper=1> theta_new[N_new];    // vector of realizations of prevalence values by site

  for (n in 1:N){
    real mu_ppc = X[,n]' * beta + b0;         // generating function to describe realizations of logit number of diseased colonies 
    theta[n] = inv_logit(mu_ppc); 
    Y_ppc[n] = binomial_rng(C[n], theta[n]);  // realizations of number of diseased colonies
   }
  
  // predict on new data
  for (w in 1:N_new){
    real mu_new = X_new[,w]' * beta + b0;         // generating function to describe realizations of logit number of diseased colonies 
    theta_new[w] = inv_logit(mu_new);             // prevalence
   }
}

