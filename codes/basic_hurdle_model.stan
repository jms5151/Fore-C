// this function counts the number of non-zero (nz) numbers in data
functions {
  int num_zero(int[] y) {
    int nz = 0;
    for (n in 1:size(y))
      if (y[n] == 0)
        nz += 1;
    return nz;
  }
}

data {
  int<lower=0> N;     // count of number of datapoints 
  int<lower=0> y[N];  // vector of length N
}

transformed data {
  int<lower=0, upper=N> N0 = num_zero(y); // count of number of zeros
  int<lower=0, upper=N> Ngt0 = N - N0;    // count of numbers greater than zero
  int<lower=1> y_nz[N - num_zero(y)];     // vector of length of positive numbers (for second part of hurdle model)
  {
    int pos = 1;
    for (n in 1:N) {
      if (y[n] != 0) {
        y_nz[pos] = y[n];
        pos += 1;
      }
    }
  }
}

parameters {
  real<lower=0, upper=1> theta;
  real<lower=0> lambda;
}

model {
  N0 ~ binomial(N, theta); // probability theta of zero
  y_nz ~ poisson(lambda);
  target += -Ngt0 * log1m_exp(-lambda); // calculating the expected number to compare with the number for the non-zero value? Target will be small when numbers of close to data and vice versa. Inverse weighting of how likely the parameters are
}

