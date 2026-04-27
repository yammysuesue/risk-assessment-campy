data {
  int<lower=1> N;                 // number of observations
  int<lower=1> K;                 // number of regression coefficients
  matrix[N, K] X;                 // design matrix
  vector[N] y;                    // observed log10 concentrations
  vector[N] lod;                  // limit of detection
  array[N] int<lower=0, upper=1> cen;   // censoring indicator
}

parameters {
  vector[K] beta;                 // regression coefficients
  real<lower=0> sigma;            // common standard deviation
}

transformed parameters {
  vector[N] mu;
  mu = X * beta;
}

model {
  // priors
  beta ~ normal(0, 5);
  sigma ~ normal(0, 2) T[0, ];

  // likelihood
  for (i in 1:N) {
    if (cen[i] == 1) {
      target += normal_lcdf(lod[i] | mu[i], sigma);
    } else {
      target += normal_lpdf(y[i] | mu[i], sigma);
    }
  }
}

