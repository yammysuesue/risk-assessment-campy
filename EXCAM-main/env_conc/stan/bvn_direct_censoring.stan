functions {
  real biv_left_cens_integrand(real z,
                               real xc,
                               array[] real theta,
                               array[] real x_r,
                               array[] int x_i) {
    real a   = theta[1];
    real rho = theta[2];
    real s   = sqrt(fmax(1e-10, 1 - square(rho)));

    return exp(
      normal_lpdf(z | 0, 1) +
      normal_lcdf((a - rho * z) / s | 0, 1)
    );
  }
}

data {
  int<lower=1> N;
  int<lower=1> S;
  array[N] int<lower=1, upper=S> source_id;

  vector[N] dna_tot;
  vector[N] dna_via;

  array[N] int<lower=0, upper=1> cens_tot;
  array[N] int<lower=0, upper=1> cens_via;

  vector[N] lod_tot;
  vector[N] lod_via;
}

parameters {
  vector[S] mu_tot;
  vector[S] mu_via;

  real<lower=1e-6> sigma_tot;
  real<lower=1e-6> sigma_via;
  real<lower=-0.98, upper=0.98> rho;
}

transformed parameters {
  cov_matrix[2] Sigma;
  Sigma[1,1] = square(sigma_tot);
  Sigma[2,2] = square(sigma_via);
  Sigma[1,2] = rho * sigma_tot * sigma_via;
  Sigma[2,1] = Sigma[1,2];
}

model {
  mu_tot ~ normal(0, 10);
  mu_via ~ normal(0, 10);
  sigma_tot ~ lognormal(0, 0.7);
  sigma_via ~ lognormal(0, 0.7);
  rho ~ normal(0, 0.5);

  for (i in 1:N) {
    int s = source_id[i];
    real mu1 = mu_tot[s];
    real mu2 = mu_via[s];

    if (cens_tot[i] == 0 && cens_via[i] == 0) {
      vector[2] y;
      vector[2] mu;
      y[1] = dna_tot[i];
      y[2] = dna_via[i];
      mu[1] = mu1;
      mu[2] = mu2;

      target += multi_normal_lpdf(y | mu, Sigma);

    } else if (cens_tot[i] == 1 && cens_via[i] == 0) {
      real mu1_given_2 =
        mu1 + rho * sigma_tot / sigma_via * (dna_via[i] - mu2);
      real sd1_given_2 =
        sigma_tot * sqrt(fmax(1e-12, 1 - square(rho)));

      target += normal_lpdf(dna_via[i] | mu2, sigma_via);
      target += normal_lcdf(lod_tot[i] | mu1_given_2, sd1_given_2);

    } else if (cens_tot[i] == 0 && cens_via[i] == 1) {
      real mu2_given_1 =
        mu2 + rho * sigma_via / sigma_tot * (dna_tot[i] - mu1);
      real sd2_given_1 =
        sigma_via * sqrt(fmax(1e-12, 1 - square(rho)));

      target += normal_lpdf(dna_tot[i] | mu1, sigma_tot);
      target += normal_lcdf(lod_via[i] | mu2_given_1, sd2_given_1);

    } else {
      real a = (lod_tot[i] - mu1) / sigma_tot;
      real b = (lod_via[i] - mu2) / sigma_via;
      array[2] real theta;
      real p_rect;

      theta[1] = a;
      theta[2] = rho;

      p_rect = integrate_1d(
        biv_left_cens_integrand,
        negative_infinity(),
        b,
        theta,
        rep_array(0.0, 0),
        rep_array(0, 0),
        1e-5
      );

      target += log(fmax(p_rect, 1e-12));
    }
  }
}

