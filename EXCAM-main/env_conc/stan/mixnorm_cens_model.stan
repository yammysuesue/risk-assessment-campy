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
    real<lower=2, upper=5> mu1;                // mean of 1st normal distribution
    real<lower=0, upper=5> delta;                // difference in mean between the two normal distributions
    real<lower=0> sigma;    // shared stdev of both normal distribution    real<lower=0, upper=1>  alpha; // mix probability}

transformed parameters {
    real mu2;                // mean of 2nd normal distribution
    mu2 = mu1 + delta;
}

model{
    mu1 ~ uniform(2, 5);         // prior for mu
    delta ~ uniform(0, 5);         // prior for mu
    sigma ~ lognormal(0, 10);  // prior for sigma
    alpha ~ beta(1, 1);  // impute missing outcomes

    for (i in 1:N){
        if(cen[i] == 1) {
          target += log_mix(alpha,
                      normal_lcdf(lod[i] | mu1, sigma),
                      normal_lcdf(lod[i] | mu2, sigma)) ;          } else {
          target += log_mix(alpha,
                      normal_lpdf(y[i] | mu1, sigma),
                      normal_lpdf(y[i] | mu2, sigma));
        }  
    }
}
