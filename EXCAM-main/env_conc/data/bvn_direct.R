library(rstan)
library(dplyr)
library(pbmcapply)

options(mc.cores = 1) # Set to 1 because we parallelize at the source level
rstan_options(auto_write = TRUE)

root_path <- "/home/ghosn/Project/EXCAM-main/env_conc"
setwd(root_path)

stan_file <- file.path(
  root_path, "stan",
  "bvn_direct_censoring.stan"
)
bvn_cens_ll_model <- stan_model(file = stan_file)

prepare_bvn_direct_source_data <- function(dat, source_name) {
  tmp <- dat %>%
    filter(
      source == source_name,
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

stanfit_has_samples <- function(fit) {
  methods::is(fit, "stanfit") &&
    fit@mode == 0 &&
    length(fit@sim$samples) > 0
}

extract_stan_error_message <- function(fit_obj) {
  if (!methods::is(fit_obj, "stanfit")) {
    return("Sampling did not return a stanfit object.")
  }

  msgs <- vapply(
    fit_obj@stan_args,
    function(x) {
      msg <- x$error_msgs
      if (is.null(msg) || length(msg) == 0) {
        ""
      } else {
        paste(msg, collapse = " ")
      }
    },
    character(1)
  )

  msgs <- unique(msgs[nzchar(msgs)])

  if (length(msgs) == 0) {
    return("Stan returned no posterior draws, but no chain error message was captured.")
  }

  paste(msgs, collapse = " | ")
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

fit_env_paras_bvn_ll_by_source <- function(dat,
                                           stan_model_obj,
                                           source_levels = sort(unique(dat$source)),
                                           chains = 4,
                                           iter = 2000,
                                           warmup = 1000,
                                           seed = 123,
                                           adapt_delta = 0.95,
                                           max_treedepth = 12,
                                           workers = 12) {
  
  message("Starting parallel processing for ", length(source_levels), " sources on ", workers, " cores...")

  # pbmclapply provides a progress bar for multicore processing on Linux
  env.paras <- pbmclapply(source_levels, function(s) {
    prepared <- prepare_bvn_direct_source_data(dat, s)

    if (is.null(prepared)) {
      return(list(
        source = s, n = 0,
        post.med = c(mu_tot = NA_real_, mu_via = NA_real_, sigma_tot = NA_real_, sigma_via = NA_real_, rho = NA_real_),
        post.cov = matrix(NA_real_, 5, 5, dimnames = list(c("mu_tot", "mu_via", "sigma_tot", "sigma_via", "rho"), c("mu_tot", "mu_via", "sigma_tot", "sigma_via", "rho"))),
        posterior_draws = NULL, sampling_error = "No usable rows for this source after filtering.", fit = NULL
      ))
    }

    tmp <- prepared$data
    stan_dat <- prepared$stan_data
    init_list <- make_bvn_init_list(tmp, chains)

    # Note: Inside parallel workers, we must run Stan chains sequentially
    # to avoid over-subscribing the CPU (Total processes = workers * 1).
    fit <- tryCatch(
      rstan::sampling(
        object = stan_model_obj,
        data = stan_dat,
        chains = chains,
        cores = 1, # Force sequential chains within each worker
        iter = iter,
        warmup = warmup,
        seed = seed,
        init = init_list,
        refresh = 500, # Show progress every 500 iterations
        show_messages = TRUE, # Enable Stan messages
        control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth)
      ),
      error = function(e) e
    )

    if (inherits(fit, "error")) {
      return(list(
        source = s, n = nrow(tmp), n_cens_tot = sum(tmp$cens_tot == 1L), n_cens_via = sum(tmp$cens_via == 1L),
        post.med = c(mu_tot = NA_real_, mu_via = NA_real_, sigma_tot = NA_real_, sigma_via = NA_real_, rho = NA_real_),
        post.cov = matrix(NA_real_, 5, 5, dimnames = list(c("mu_tot", "mu_via", "sigma_tot", "sigma_via", "rho"), c("mu_tot", "mu_via", "sigma_tot", "sigma_via", "rho"))),
        posterior_draws = NULL, sampling_error = conditionMessage(fit), fit = NULL
      ))
    }

    if (!stanfit_has_samples(fit)) {
      err_msg <- extract_stan_error_message(fit)
      return(list(
        source = s, n = nrow(tmp), n_cens_tot = sum(tmp$cens_tot == 1L), n_cens_via = sum(tmp$cens_via == 1L),
        post.med = c(mu_tot = NA_real_, mu_via = NA_real_, sigma_tot = NA_real_, sigma_via = NA_real_, rho = NA_real_),
        post.cov = matrix(NA_real_, 5, 5, dimnames = list(c("mu_tot", "mu_via", "sigma_tot", "sigma_via", "rho"), c("mu_tot", "mu_via", "sigma_tot", "sigma_via", "rho"))),
        posterior_draws = NULL, sampling_error = err_msg, fit = fit
      ))
    }

    theta <- extract_bvn_direct_draws(fit, s)

    return(list(
      source = s, n = nrow(tmp), n_cens_tot = sum(tmp$cens_tot == 1L), n_cens_via = sum(tmp$cens_via == 1L),
      post.med = c(mu_tot = median(theta$mu_tot), mu_via = median(theta$mu_via), sigma_tot = median(theta$sigma_tot), sigma_via = median(theta$sigma_via), rho = median(theta$rho)),
      post.cov = cov(theta[, c("mu_tot", "mu_via", "sigma_tot", "sigma_via", "rho")]),
      posterior_draws = theta, sampling_error = NULL, fit = fit
    ))
  }, mc.cores = workers)
  
  names(env.paras) <- source_levels
  return(env.paras)
}

source_levels <- c("EB", "EF", "EV", "HA", "HB", "HH", "HP", "HR")

if (!dir.exists(file.path(root_path, "data/output"))) {
  dir.create(file.path(root_path, "data/output"), recursive = TRUE)
}

env.paras_bvn_ll_all <- fit_env_paras_bvn_ll_by_source(
  dat = df_all,
  stan_model_obj = bvn_cens_ll_model,
  source_levels = source_levels,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 123
)

post_med_df <- do.call(
  rbind,
  lapply(env.paras_bvn_ll_all, function(x) {
    mu_tot <- if (!is.null(x$post.med)) x$post.med["mu_tot"] else NA_real_
    mu_via <- if (!is.null(x$post.med)) x$post.med["mu_via"] else NA_real_
    sigma_tot <- if (!is.null(x$post.med)) x$post.med["sigma_tot"] else NA_real_
    sigma_via <- if (!is.null(x$post.med)) x$post.med["sigma_via"] else NA_real_
    rho <- if (!is.null(x$post.med)) x$post.med["rho"] else NA_real_
    
    data.frame(
      source = x$source,
      n = x$n,
      n_cens_tot = if (!is.null(x$n_cens_tot)) x$n_cens_tot else NA_integer_,
      n_cens_via = if (!is.null(x$n_cens_via)) x$n_cens_via else NA_integer_,
      mu_tot = as.numeric(mu_tot),
      mu_via = as.numeric(mu_via),
      sigma_tot = as.numeric(sigma_tot),
      sigma_via = as.numeric(sigma_via),
      rho = as.numeric(rho),
      sampling_error = if (!is.null(x$sampling_error)) x$sampling_error else NA_character_,
      stringsAsFactors = FALSE
    )
  })
)

save(env.paras_bvn_ll_all, file = file.path(root_path, "data/output/env.paras_bvn_ll_all.rda"))
save(post_med_df, file = file.path(root_path, "data/output/post_med_df.rda"))

post_med_df


#check traceplot
library(bayesplot)
library(ggplot2)

sources <- names(env.paras_bvn_ll_all)
target_pars <- c("mu_tot[1]", "mu_via[1]", "sigma_tot", "sigma_via", "rho")

for (s in sources) {
  cat("\n===== source:", s, "=====\n")
  
  fit <- env.paras_bvn_ll_all[[s]]$fit
  
  # 1. 跳过 NULL fit
  if (is.null(fit)) {
    cat("Skipped:", s, "- fit is NULL\n")
    next
  }
  
  # 2. 提取参数名
  param_names <- tryCatch(
    dimnames(as.array(fit))[[3]],
    error = function(e) NULL
  )
  
  if (is.null(param_names)) {
    cat("Skipped:", s, "- cannot extract parameter names\n")
    next
  }
  
  # 3. 只保留存在的目标参数
  use_pars <- intersect(target_pars, param_names)
  print(use_pars)
  
  # 4. 如果没有可画参数，跳过
  if (length(use_pars) == 0) {
    cat("Skipped:", s, "- no matching parameters found\n")
    next
  }
  
  # 5. 安全绘图
  p <- tryCatch(
    mcmc_trace(as.array(fit), pars = use_pars) +
      ggtitle(paste("Traceplot -", s)),
    error = function(e) {
      cat("Plot failed for", s, ":", conditionMessage(e), "\n")
      NULL
    }
  )
  
  # 6. 只有在 p 不是 NULL 时才 print
  if (!is.null(p)) {
    print(p)
  }
}