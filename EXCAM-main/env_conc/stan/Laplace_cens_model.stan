// From https://github.com/peterkomar-hu/censoring-w-stan/blob/master/stan_code/1D-censored-normal.stan

// This STAN model implements a 1D censored normal distribution
//
// Input data:
//      * 1D array of real numbers
//
// Parameters:
//      * mu: mean of the normal distribution
//      * sigma: stdev of the normal distribution


data {
    int<lower=10> N;  
    real<lower=0> lod[N];
    real<lower=0> y[N];    int  cen[N];
}

parameters {
    real mu;                // mean of normal distribution
    real<lower=0> sigma;    // stdev of normal distribution
}

model{
    mu ~ normal(0, 10);         // prior for mu
    sigma ~ lognormal(0, 10);  // prior for sigma
    
    for (i in 1:N){
        if(cen[i] == 1) {
          target += double_exponential_lcdf(lod[i] | mu, sigma);          } else {
          target += double_exponential_lpdf(y[i] | mu, sigma);
        }  
    }
}
