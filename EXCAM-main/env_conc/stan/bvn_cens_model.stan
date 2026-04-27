data {
  int<lower=1> N;                    // number of observations
  int<lower=1> S;                    // number of sources

  array[N] int<lower=1, upper=S> source_id;

  // observed log10 DNA values
  // for censored rows these can be placeholders; they will be overwritten
  array[N] real dna_tot;
  array[N] real dna_via;

  // censoring indicators: 1 = censored, 0 = observed
  array[N] int<lower=0, upper=1> cens_tot;
  array[N] int<lower=0, upper=1> cens_via;

  // log10 LODs
  array[N] real lod_tot;
  array[N] real lod_via;

  // indices of censored rows
  int<lower=0> N_tot_cens;
  int<lower=0> N_via_cens;

  array[N_tot_cens] int<lower=1, upper=N> idx_tot_cens;
  array[N_via_cens] int<lower=1, upper=N> idx_via_cens;
}

parameters {
  // source-specific means on log10 scale
  vector[S] mu_tot;
  vector[S] mu_via;

  // shared covariance parameters
  real<lower=0> sigma_tot;
  real<lower=0> sigma_via;
  real<lower=-1, upper=1> rho;

  // latent censored values via unconstrained parameters
  vector[N_tot_cens] z_tot_cens;
  vector[N_via_cens] z_via_cens;
}

transformed parameters {
  array[N] real y_tot;
  array[N] real y_via;

  matrix[2,2] Sigma;

  Sigma[1,1] = square(sigma_tot);
  Sigma[2,2] = square(sigma_via);
  Sigma[1,2] = rho * sigma_tot * sigma_via;
  Sigma[2,1] = Sigma[1,2];

  // start with supplied values
  for (i in 1:N) {
    y_tot[i] = dna_tot[i];
    y_via[i] = dna_via[i];
  }

  // replace censored values with latent values satisfying y < lod
  for (k in 1:N_tot_cens) {
    int i = idx_tot_cens[k];
    y_tot[i] = lod_tot[i] - exp(z_tot_cens[k]);
  }

  for (k in 1:N_via_cens) {
    int i = idx_via_cens[k];
    y_via[i] = lod_via[i] - exp(z_via_cens[k]);
  }
}

model {
  vector[2] mu_i;
  vector[2] y_i;

  // priors
  mu_tot ~ normal(0, 3);
  mu_via ~ normal(0, 3);

  sigma_tot ~ normal(0, 2);
  sigma_via ~ normal(0, 2);

  rho ~ uniform(-1, 1);

  // Jacobian adjustments for:
  // y = lod - exp(z)
  for (k in 1:N_tot_cens) {
    target += z_tot_cens[k];
  }

  for (k in 1:N_via_cens) {
    target += z_via_cens[k];
  }

  // row-wise bivariate normal likelihood
  for (i in 1:N) {
    mu_i[1] = mu_tot[source_id[i]];
    mu_i[2] = mu_via[source_id[i]];

    y_i[1] = y_tot[i];
    y_i[2] = y_via[i];

    y_i ~ multi_normal(mu_i, Sigma);
  }
}

generated quantities {
  corr_matrix[2] R;
  cov_matrix[2] Sigma_out;

  R[1,1] = 1;
  R[1,2] = rho;
  R[2,1] = rho;
  R[2,2] = 1;

  Sigma_out[1,1] = square(sigma_tot);
  Sigma_out[2,2] = square(sigma_via);
  Sigma_out[1,2] = rho * sigma_tot * sigma_via;
  Sigma_out[2,1] = Sigma_out[1,2];
}

