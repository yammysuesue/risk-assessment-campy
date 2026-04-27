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
    real lod[N];
    real y[N];    int  cen[N];
}

parameters {
    real<lower=2, upper=6> mu;                // mean of normal distribution
    real<lower=0> sigma;    // stdev of normal distribution
    real<lower=0, upper=10> nu;
}

model{
    mu ~ uniform(2, 6);         // prior for mu
    sigma ~ lognormal(0, 10);  // prior for sigma
    nu ~ uniform(0, 10);  // prior for degree of freedom nu
    
    for (i in 1:N){
        if(cen[i] == 1) {
          target += student_t_lcdf(lod[i] | nu, mu, sigma);          } else {
          target += student_t_lpdf(y[i] | nu, mu, sigma);
        }  
    }
}
