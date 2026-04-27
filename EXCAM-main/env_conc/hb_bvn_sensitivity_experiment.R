library(rstan)
library(dplyr)
library(tidyr)
library(ggplot2)

options(mc.cores = 1)
rstan_options(auto_write = TRUE)

## Assumes `df_all` is already available in the current R session.
## The model below is equivalent to `stan/bvn_direct_censoring.stan` but is
## written in legacy Stan array syntax to be more robust in RStan setups that
## struggle with the newer parser.
hb_bvn_stan_code <- "
functions {
  real biv_left_cens_integrand(real z,
                               real xc,
                               real[] theta,
                               real[] x_r,
                               int[] x_i) {
    real a;
    real rho;
    real s;
    a = theta[1];
    rho = theta[2];
    s = sqrt(fmax(1e-10, 1 - square(rho)));

    return exp(
      normal_lpdf(z | 0, 1) +
      normal_lcdf((a - rho * z) / s | 0, 1)
    );
  }
}

data {
  int<lower=1> N;
  int<lower=1> S;
  int<lower=1, upper=S> source_id[N];

  vector[N] dna_tot;
  vector[N] dna_via;

  int<lower=0, upper=1> cens_tot[N];
  int<lower=0, upper=1> cens_via[N];

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
    int s;
    real mu1;
    real mu2;
    s = source_id[i];
    mu1 = mu_tot[s];
    mu2 = mu_via[s];

    if (cens_tot[i] == 0 && cens_via[i] == 0) {
      vector[2] y;
      vector[2] mu;
      y[1] = dna_tot[i];
      y[2] = dna_via[i];
      mu[1] = mu1;
      mu[2] = mu2;

      target += multi_normal_lpdf(y | mu, Sigma);

    } else if (cens_tot[i] == 1 && cens_via[i] == 0) {
      real mu1_given_2;
      real sd1_given_2;
      mu1_given_2 =
        mu1 + rho * sigma_tot / sigma_via * (dna_via[i] - mu2);
      sd1_given_2 =
        sigma_tot * sqrt(fmax(1e-12, 1 - square(rho)));

      target += normal_lpdf(dna_via[i] | mu2, sigma_via);
      target += normal_lcdf(lod_tot[i] | mu1_given_2, sd1_given_2);

    } else if (cens_tot[i] == 0 && cens_via[i] == 1) {
      real mu2_given_1;
      real sd2_given_1;
      mu2_given_1 =
        mu2 + rho * sigma_via / sigma_tot * (dna_tot[i] - mu1);
      sd2_given_1 =
        sigma_via * sqrt(fmax(1e-12, 1 - square(rho)));

      target += normal_lpdf(dna_tot[i] | mu1, sigma_tot);
      target += normal_lcdf(lod_via[i] | mu2_given_1, sd2_given_1);

    } else {
      real a;
      real b;
      real theta[2];
      real p_rect;
      a = (lod_tot[i] - mu1) / sigma_tot;
      b = (lod_via[i] - mu2) / sigma_via;

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
"

hb_bvn_model <- stan_model(
  model_code = hb_bvn_stan_code,
  model_name = "hb_bvn_sensitivity"
)

hb_bvn_source <- "HB"
hb_bvn_seed <- 123
hb_bvn_chains <- 4
hb_bvn_iter <- 2000
hb_bvn_warmup <- 1000
hb_bvn_adapt_delta <- 0.95
hb_bvn_max_treedepth <- 12
hb_bvn_pred_draws <- 5000
hb_bvn_output_dir <- getwd()

hb_bvn_scenarios <- list(
  original = c(3.21, 3.45),
  spread_090 = c(3.00, 3.90),
  spread_150 = c(2.95, 4.45),
  spread_210 = c(3.05, 5.15)
)

hb_df <- df_all %>%
  filter(as.character(source) == hb_bvn_source)

uncens_tot_idx_local <- which(hb_df$cens_tot == 0L)

if (length(uncens_tot_idx_local) != 2) {
  stop("Expected exactly 2 uncensored total-DNA rows for ", hb_bvn_source,
       ", found ", length(uncens_tot_idx_local), ".")
}

hb_original_tot_values <- hb_df$dna_tot[uncens_tot_idx_local]
hb_uncens_tot_lods <- hb_df$dna_tot_lod[uncens_tot_idx_local]

validate_scenarios <- function(scenarios, lod_values) {
  lod_values <- as.numeric(lod_values)
  bad_lengths <- names(scenarios)[vapply(scenarios, length, integer(1)) != 2L]
  if (length(bad_lengths) > 0) {
    stop("Each scenario must define exactly two replacement values. Bad scenarios: ",
         paste(bad_lengths, collapse = ", "))
  }

  for (nm in names(scenarios)) {
    vals <- as.numeric(scenarios[[nm]])
    if (any(!is.finite(vals))) {
      stop("Scenario ", nm, " contains non-finite values.")
    }
    below_lod <- vals <= lod_values[seq_along(vals)]
    if (any(below_lod, na.rm = TRUE)) {
      warning(
        paste0(
          "Scenario ", nm,
          " contains one or more replacement values that are not above the corresponding LODs (",
          paste(round(lod_values, 2), collapse = ", "),
          "). The fit will still run, but that scenario is inconsistent with uncensored total-DNA observations."
        ),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}

validate_scenarios(hb_bvn_scenarios, hb_uncens_tot_lods)

prepare_bvn_direct_source_data <- function(dat, source_name) {
  tmp <- dat %>%
    filter(
      as.character(source) == source_name,
      !is.na(cens_tot), !is.na(cens_via),
      !is.na(dna_tot_lod), !is.na(dna_via_lod)
    ) %>%
    mutate(
      cens_tot = as.integer(cens_tot),
      cens_via = as.integer(cens_via)
    ) %>%
    filter(
      cens_tot %in% c(0L, 1L),
      cens_via %in% c(0L, 1L)
    ) %>%
    filter(
      !(cens_tot == 0L & is.na(dna_tot)),
      !(cens_via == 0L & is.na(dna_via))
    ) %>%
    mutate(
      dna_tot_stan = if_else(is.na(dna_tot), dna_tot_lod, dna_tot),
      dna_via_stan = if_else(is.na(dna_via), dna_via_lod, dna_via)
    )

  if (nrow(tmp) == 0) {
    return(NULL)
  }

  list(
    data = tmp,
    stan_data = list(
      N = nrow(tmp),
      S = 1L,
      source_id = rep(1L, nrow(tmp)),
      dna_tot = as.numeric(tmp$dna_tot_stan),
      dna_via = as.numeric(tmp$dna_via_stan),
      cens_tot = as.integer(tmp$cens_tot),
      cens_via = as.integer(tmp$cens_via),
      lod_tot = as.numeric(tmp$dna_tot_lod),
      lod_via = as.numeric(tmp$dna_via_lod)
    )
  )
}

extract_bvn_direct_draws <- function(fit, source_name) {
  post <- rstan::extract(
    fit,
    pars = c("mu_tot", "mu_via", "sigma_tot", "sigma_via", "rho")
  )

  data.frame(
    source = source_name,
    draw = seq_along(post$rho),
    mu_tot = post$mu_tot[, 1],
    mu_via = post$mu_via[, 1],
    sigma_tot = post$sigma_tot,
    sigma_via = post$sigma_via,
    rho = post$rho,
    row.names = NULL
  )
}

make_bvn_init_list <- function(tmp, chains) {
  mu_tot_init <- mean(tmp$dna_tot_stan, na.rm = TRUE)
  mu_via_init <- mean(tmp$dna_via_stan, na.rm = TRUE)
  sigma_tot_init <- max(stats::sd(tmp$dna_tot_stan, na.rm = TRUE), 0.2)
  sigma_via_init <- max(stats::sd(tmp$dna_via_stan, na.rm = TRUE), 0.2)

  lapply(seq_len(chains), function(k) {
    jitter_scale <- 0.05 * (k - (chains + 1) / 2)
    list(
      mu_tot = as.array(mu_tot_init + jitter_scale),
      mu_via = as.array(mu_via_init - jitter_scale),
      sigma_tot = sigma_tot_init,
      sigma_via = sigma_via_init,
      rho = 0
    )
  })
}

fit_single_source_bvn <- function(dat,
                                  source_name,
                                  stan_model_obj,
                                  chains = hb_bvn_chains,
                                  iter = hb_bvn_iter,
                                  warmup = hb_bvn_warmup,
                                  seed = hb_bvn_seed,
                                  adapt_delta = hb_bvn_adapt_delta,
                                  max_treedepth = hb_bvn_max_treedepth) {
  prepared <- prepare_bvn_direct_source_data(dat, source_name)

  if (is.null(prepared)) {
    stop("No usable rows after filtering for source ", source_name)
  }

  run_sampling <- function(model_obj) {
    rstan::sampling(
      object = model_obj,
      data = prepared$stan_data,
      chains = chains,
      cores = 1,
      iter = iter,
      warmup = warmup,
      seed = seed,
      init = make_bvn_init_list(prepared$data, chains),
      refresh = 0,
      control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth)
    )
  }

  fit <- tryCatch(
    run_sampling(stan_model_obj),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("compiled object.*invalid", msg, ignore.case = TRUE)) {
        message("Cached Stan model is invalid; recompiling embedded Stan model")
        fresh_model <- stan_model(
          model_code = hb_bvn_stan_code,
          model_name = "hb_bvn_sensitivity_recompiled"
        )
        return(run_sampling(fresh_model))
      }
      stop(e)
    }
  )

  post_draws <- extract_bvn_direct_draws(fit, source_name)

  list(
    fit = fit,
    data = prepared$data,
    posterior_draws = post_draws,
    post_med = c(
      mu_tot = median(post_draws$mu_tot),
      mu_via = median(post_draws$mu_via),
      sigma_tot = median(post_draws$sigma_tot),
      sigma_via = median(post_draws$sigma_via),
      rho = median(post_draws$rho)
    )
  )
}

make_observed_long <- function(prepared_data, scenario_name, scenario_values) {
  bind_rows(
    prepared_data %>%
      transmute(
        scenario = scenario_name,
        assay = "Total DNA",
        value = dna_tot_stan,
        lod = dna_tot_lod,
        cens = cens_tot,
        cens_grp = if_else(cens_tot == 1L, "Censored", "Observed")
      ),
    prepared_data %>%
      transmute(
        scenario = scenario_name,
        assay = "Viable DNA",
        value = dna_via_stan,
        lod = dna_via_lod,
        cens = cens_via,
        cens_grp = if_else(cens_via == 1L, "Censored", "Observed")
      )
  ) %>%
    mutate(
      scenario_values = paste(sprintf("%.2f", scenario_values), collapse = ", ")
    )
}

make_posterior_long <- function(post_draws, scenario_name, scenario_values, n_draws = hb_bvn_pred_draws) {
  idx <- sample(
    seq_len(nrow(post_draws)),
    size = n_draws,
    replace = nrow(post_draws) < n_draws
  )

  bind_rows(
    data.frame(
      scenario = scenario_name,
      assay = "Total DNA",
      draw_value = rnorm(
        n_draws,
        mean = post_draws$mu_tot[idx],
        sd = pmax(post_draws$sigma_tot[idx], 1e-6)
      ),
      scenario_values = paste(sprintf("%.2f", scenario_values), collapse = ", "),
      stringsAsFactors = FALSE
    ),
    data.frame(
      scenario = scenario_name,
      assay = "Viable DNA",
      draw_value = rnorm(
        n_draws,
        mean = post_draws$mu_via[idx],
        sd = pmax(post_draws$sigma_via[idx], 1e-6)
      ),
      scenario_values = paste(sprintf("%.2f", scenario_values), collapse = ", "),
      stringsAsFactors = FALSE
    )
  )
}

make_hist_df <- function(df, n_bins = 20, min_binwidth = 0.25) {
  df %>%
    group_by(scenario, assay) %>%
    group_modify(~{
      x <- .x$value
      n_total <- sum(is.finite(x))
      rng <- range(x, na.rm = TRUE)

      if (!all(is.finite(rng)) || n_total == 0) {
        return(tibble::tibble())
      }
      if (diff(rng) == 0) {
        rng <- rng + c(-0.5, 0.5)
      }

      bw <- max(diff(rng) / n_bins, min_binwidth)
      breaks <- seq(
        from = floor(rng[1] / bw) * bw,
        to = ceiling(rng[2] / bw) * bw,
        by = bw
      )

      if (length(breaks) < 2) {
        breaks <- c(rng[1] - bw / 2, rng[2] + bw / 2)
      }

      n_breaks <- length(breaks) - 1

      .x %>%
        mutate(
          bin = cut(
            value,
            breaks = breaks,
            include.lowest = TRUE,
            right = TRUE,
            labels = FALSE
          )
        ) %>%
        filter(!is.na(bin)) %>%
        count(bin, cens_grp, name = "count") %>%
        complete(
          bin = 1:n_breaks,
          cens_grp = c("Observed", "Censored"),
          fill = list(count = 0)
        ) %>%
        mutate(
          xmin = breaks[bin],
          xmax = breaks[bin + 1],
          density = count / (n_total * bw)
        ) %>%
        arrange(bin, factor(cens_grp, levels = c("Observed", "Censored"))) %>%
        group_by(bin) %>%
        mutate(
          ymin = c(0, cumsum(density)[-length(density)]),
          ymax = cumsum(density)
        ) %>%
        ungroup()
    }) %>%
    ungroup()
}

make_lod_df <- function(observed_long) {
  observed_long %>%
    filter(cens == 1L, !is.na(lod), is.finite(lod)) %>%
    group_by(scenario, assay, scenario_values) %>%
    summarise(
      max_lod = max(lod, na.rm = TRUE),
      .groups = "drop"
    )
}

make_single_scenario_plot <- function(hist_df, posterior_df, lod_df, summary_row) {
  ggplot() +
    geom_rect(
      data = hist_df,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = cens_grp
      ),
      color = "white",
      linewidth = 0.2,
      alpha = 0.85
    ) +
    geom_density(
      data = posterior_df,
      aes(x = draw_value),
      color = "black",
      linewidth = 1
    ) +
    geom_vline(
      data = lod_df,
      aes(xintercept = max_lod),
      linetype = "dashed",
      color = "blue",
      linewidth = 0.7
    ) +
    geom_text(
      data = lod_df,
      aes(x = max_lod, y = 0, label = "max LOD"),
      color = "blue",
      angle = 90,
      vjust = -0.4,
      size = 3,
      inherit.aes = FALSE
    ) +
    facet_wrap(~ assay, scales = "free_x", ncol = 2) +
    scale_fill_manual(
      values = c(
        "Observed" = "grey75",
        "Censored" = "#E69F00"
      )
    ) +
    labs(
      title = paste0("HB BVN sensitivity: ", summary_row$scenario),
      subtitle = paste0(
        "modified uncensored total values = ", summary_row$scenario_values,
        " | posterior medians: mu_tot = ", sprintf("%.2f", summary_row$mu_tot),
        ", mu_via = ", sprintf("%.2f", summary_row$mu_via),
        ", rho = ", sprintf("%.2f", summary_row$rho)
      ),
      x = expression(log[10](DNA)),
      y = "Density",
      fill = NULL
    ) +
    theme_bw() +
    coord_cartesian(clip = "off")
}

run_hb_scenario <- function(scenario_name, replacement_values) {
  dat_mod <- df_all
  hb_rows_global <- which(as.character(dat_mod$source) == hb_bvn_source)
  uncens_tot_idx_global <- hb_rows_global[which(dat_mod$cens_tot[hb_rows_global] == 0L)]
  dat_mod$dna_tot[uncens_tot_idx_global] <- as.numeric(replacement_values)

  fit_res <- fit_single_source_bvn(
    dat = dat_mod,
    source_name = hb_bvn_source,
    stan_model_obj = hb_bvn_model
  )

  list(
    scenario = scenario_name,
    replacement_values = as.numeric(replacement_values),
    fit = fit_res,
    observed_long = make_observed_long(fit_res$data, scenario_name, replacement_values),
    posterior_long = make_posterior_long(fit_res$posterior_draws, scenario_name, replacement_values),
    summary = data.frame(
      scenario = scenario_name,
      scenario_values = paste(sprintf("%.2f", replacement_values), collapse = ", "),
      mu_tot = unname(fit_res$post_med["mu_tot"]),
      mu_via = unname(fit_res$post_med["mu_via"]),
      sigma_tot = unname(fit_res$post_med["sigma_tot"]),
      sigma_via = unname(fit_res$post_med["sigma_via"]),
      rho = unname(fit_res$post_med["rho"]),
      stringsAsFactors = FALSE
    )
  )
}

scenario_results <- lapply(names(hb_bvn_scenarios), function(nm) {
  message("Running scenario: ", nm, " with values ", paste(hb_bvn_scenarios[[nm]], collapse = ", "))
  run_hb_scenario(nm, hb_bvn_scenarios[[nm]])
})
names(scenario_results) <- names(hb_bvn_scenarios)

observed_long_all <- bind_rows(lapply(scenario_results, `[[`, "observed_long"))
posterior_long_all <- bind_rows(lapply(scenario_results, `[[`, "posterior_long"))
summary_df <- bind_rows(lapply(scenario_results, `[[`, "summary"))
lod_df_all <- make_lod_df(observed_long_all)
hist_df_all <- make_hist_df(observed_long_all, n_bins = 20, min_binwidth = 0.25)

overview_plot <- ggplot() +
  geom_rect(
    data = hist_df_all,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = cens_grp
    ),
    color = "white",
    linewidth = 0.2,
    alpha = 0.85
  ) +
  geom_density(
    data = posterior_long_all,
    aes(x = draw_value),
    color = "black",
    linewidth = 0.9
  ) +
  geom_vline(
    data = lod_df_all,
    aes(xintercept = max_lod),
    linetype = "dashed",
    color = "blue",
    linewidth = 0.6
  ) +
  facet_grid(scenario ~ assay, scales = "free_x") +
  scale_fill_manual(
    values = c(
      "Observed" = "grey75",
      "Censored" = "#E69F00"
    )
  ) +
  labs(
    title = "HB BVN sensitivity experiment",
    subtitle = paste0(
      "Original uncensored total values = ",
      paste(sprintf("%.2f", hb_original_tot_values), collapse = ", ")
    ),
    x = expression(log[10](DNA)),
    y = "Density",
    fill = NULL
  ) +
  theme_bw()

print(overview_plot)

ggsave(
  filename = file.path(hb_bvn_output_dir, "hb_bvn_sensitivity_overview.png"),
  plot = overview_plot,
  width = 9,
  height = 9,
  dpi = 300
)

for (nm in names(scenario_results)) {
  scenario_hist <- hist_df_all %>% filter(scenario == nm)
  scenario_post <- posterior_long_all %>% filter(scenario == nm)
  scenario_lod <- lod_df_all %>% filter(scenario == nm)
  scenario_summary <- summary_df %>% filter(scenario == nm)

  p <- make_single_scenario_plot(
    hist_df = scenario_hist,
    posterior_df = scenario_post,
    lod_df = scenario_lod,
    summary_row = scenario_summary
  )

  print(p)

  ggsave(
    filename = file.path(hb_bvn_output_dir, paste0("hb_bvn_", nm, ".png")),
    plot = p,
    width = 8,
    height = 4.8,
    dpi = 300
  )
}

write.csv(
  summary_df,
  file = file.path(hb_bvn_output_dir, "hb_bvn_sensitivity_summary.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    original_tot_values = hb_original_tot_values,
    total_lods = hb_uncens_tot_lods,
    scenarios = hb_bvn_scenarios,
    summary = summary_df,
    results = scenario_results
  ),
  file = file.path(hb_bvn_output_dir, "hb_bvn_sensitivity_results.rds")
)

print(summary_df)
