library(dplyr)
library(tidyr)
library(ggplot2)
library(fitdistrplus)
library(MASS)

source_spec <- data.frame(
  source = c("EB", "HH", "ED", "ES"),
  source_label = c("bathing water", "sibling handrinse", "drinking water", "soil"),
  dataset_name = c("df_all", "df_all", "df_via", "df_via"),
  value_col = c("dna_via", "dna_via", "dna", "dna"),
  cens_col = c("cens_via", "cens_via", "censored", "censored"),
  lod_col = c("dna_via_lod", "dna_via_lod", "dna_lod", "dna_lod"),
  stringsAsFactors = FALSE
)

get_dataset <- function(dataset_name) {
  switch(
    dataset_name,
    df_all = df_all,
    df_via = df_via,
    stop("Unknown dataset: ", dataset_name)
  )
}

prepare_source_data <- function(dat, source_name, value_col, cens_col, lod_col) {
  tmp <- data.frame(
    source = as.character(dat$source),
    dna = as.numeric(dat[[value_col]]),
    cens = as.integer(dat[[cens_col]]),
    lod = as.numeric(dat[[lod_col]]),
    stringsAsFactors = FALSE
  )

  tmp <- tmp[tmp$source == source_name, , drop = FALSE]

  if (nrow(tmp) == 0) {
    stop("No rows found for source ", source_name)
  }

  tmp$dna[!is.finite(tmp$dna)] <- NA_real_
  tmp$lod[!is.finite(tmp$lod)] <- NA_real_

  # For censored rows, the reported value equals the row-specific LOD when no
  # separate LOD value is available.
  tmp$lod <- ifelse(
    !is.na(tmp$lod),
    tmp$lod,
    ifelse(tmp$cens == 1L & !is.na(tmp$dna), tmp$dna, NA_real_)
  )

  tmp$dna <- ifelse(
    is.na(tmp$dna) & tmp$cens == 1L & !is.na(tmp$lod),
    tmp$lod,
    tmp$dna
  )

  tmp <- tmp[
    !is.na(tmp$cens) &
      tmp$cens %in% c(0L, 1L) &
      !is.na(tmp$dna) &
      is.finite(tmp$dna) &
      !(tmp$cens == 1L & (is.na(tmp$lod) | !is.finite(tmp$lod))),
    ,
    drop = FALSE
  ]

  tmp$left <- ifelse(tmp$cens == 1L, NA_real_, tmp$dna)
  tmp$right <- ifelse(tmp$cens == 1L, tmp$lod, tmp$dna)

  tmp
}

sample_fit_draws <- function(fit_obj, n_draws = 5000) {
  fit_sum <- summary(fit_obj)
  est <- fit_sum$estimate[c("mean", "sd")]
  vc <- fit_sum$vcov[c("mean", "sd"), c("mean", "sd"), drop = FALSE]

  param_draws <- tryCatch(
    {
      MASS::mvrnorm(n = n_draws * 4, mu = est, Sigma = vc)
    },
    error = function(e) {
      cbind(
        mean = rep(est["mean"], n_draws),
        sd = rep(est["sd"], n_draws)
      )
    }
  )

  param_draws <- as.data.frame(param_draws)
  names(param_draws) <- c("mean", "sd")

  param_draws <- param_draws %>%
    filter(
      is.finite(mean),
      is.finite(sd),
      sd > 1e-6
    )

  if (nrow(param_draws) == 0) {
    param_draws <- data.frame(
      mean = rep(est["mean"], n_draws),
      sd = rep(max(est["sd"], 1e-6), n_draws)
    )
  }

  idx <- sample(seq_len(nrow(param_draws)), size = n_draws, replace = nrow(param_draws) < n_draws)
  param_draws <- param_draws[idx, , drop = FALSE]

  rnorm(
    n = n_draws,
    mean = param_draws$mean,
    sd = pmax(param_draws$sd, 1e-6)
  )
}

get_bvn_fit_list <- function() {
  if (exists("env.paras_bvn_ll_all", inherits = TRUE)) {
    return(get("env.paras_bvn_ll_all", inherits = TRUE))
  }

  candidate_paths <- c(
    file.path("env_conc", "data", "output", "env.paras_bvn_ll_all.rda"),
    file.path("..", "..", "env_conc", "data", "output", "env.paras_bvn_ll_all.rda")
  )
  existing_path <- candidate_paths[file.exists(candidate_paths)]

  if (length(existing_path) == 0) {
    return(NULL)
  }

  e <- new.env(parent = emptyenv())
  load(existing_path[1], envir = e)

  if (!exists("env.paras_bvn_ll_all", envir = e, inherits = FALSE)) {
    return(NULL)
  }

  e$env.paras_bvn_ll_all
}

sample_bvn_fit_draws <- function(post_draws, n_draws = 5000) {
  if (is.null(post_draws) || nrow(post_draws) == 0) {
    return(numeric(0))
  }

  post_draws <- post_draws %>%
    filter(
      is.finite(mu_via),
      is.finite(sigma_via),
      sigma_via > 1e-6
    )

  if (nrow(post_draws) == 0) {
    return(numeric(0))
  }

  idx <- sample(
    seq_len(nrow(post_draws)),
    size = n_draws,
    replace = nrow(post_draws) < n_draws
  )

  rnorm(
    n = n_draws,
    mean = post_draws$mu_via[idx],
    sd = pmax(post_draws$sigma_via[idx], 1e-6)
  )
}

build_bvn_overlay_df <- function(source_spec, n_draws = 5000) {
  bvn_fit_list <- get_bvn_fit_list()

  if (is.null(bvn_fit_list)) {
    message("BVN posterior draws not found; BVN overlays will be skipped.")
    return(data.frame())
  }

  target_sources <- c("EB", "HH")

  out <- lapply(target_sources, function(src) {
    if (!src %in% names(bvn_fit_list)) {
      return(NULL)
    }

    post_draws <- bvn_fit_list[[src]]$posterior_draws
    fit_conc <- sample_bvn_fit_draws(post_draws, n_draws = n_draws)

    if (length(fit_conc) == 0) {
      return(NULL)
    }

    data.frame(
      source = src,
      source_label = source_spec$source_label[source_spec$source == src][1],
      fit_conc = fit_conc,
      model = "BVN fit",
      stringsAsFactors = FALSE
    )
  })

  bind_rows(out)
}

fit_source_model <- function(spec_row, n_draws = 5000) {
  dat <- get_dataset(spec_row$dataset_name)
  obs <- prepare_source_data(
    dat = dat,
    source_name = spec_row$source,
    value_col = spec_row$value_col,
    cens_col = spec_row$cens_col,
    lod_col = spec_row$lod_col
  )

  fit_obj <- fitdistcens(obs[, c("left", "right")], "norm")
  fit_sum <- summary(fit_obj)

  obs$source_label <- spec_row$source_label
  obs$cens_grp <- ifelse(obs$cens == 1L, "Censored", "Observed")

  fit_draws <- data.frame(
    source = spec_row$source,
    source_label = spec_row$source_label,
    fit_conc = sample_fit_draws(fit_obj, n_draws = n_draws),
    model = "UVN fit",
    stringsAsFactors = FALSE
  )

  fit_summary <- data.frame(
    source = spec_row$source,
    source_label = spec_row$source_label,
    dataset_name = spec_row$dataset_name,
    n = nrow(obs),
    n_observed = sum(obs$cens == 0L),
    n_censored = sum(obs$cens == 1L),
    mean_est = unname(fit_sum$estimate["mean"]),
    sd_est = unname(fit_sum$estimate["sd"]),
    stringsAsFactors = FALSE
  )

  list(
    observed = obs,
    fit_draws = fit_draws,
    fit_summary = fit_summary,
    fit_object = fit_obj
  )
}

make_hist_df <- function(df, n_bins = 20, min_binwidth = 0.25) {
  df %>%
    group_by(source) %>%
    group_modify(~{
      x <- .x$dna
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
            dna,
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

make_source_plot <- function(hist_data,
                             fit_data,
                             lod_data,
                             title_text,
                             facet = FALSE) {
  p <- ggplot() +
    geom_rect(
      data = hist_data,
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
      data = fit_data,
      aes(x = fit_conc, color = model),
      linewidth = 1
    ) +
    geom_vline(
      data = lod_data,
      aes(xintercept = max_lod),
      linetype = "dashed",
      color = "blue",
      linewidth = 0.7
    ) +
    geom_text(
      data = lod_data,
      aes(x = max_lod, y = 0, label = "max LOD"),
      color = "blue",
      angle = 90,
      vjust = -0.4,
      size = 3,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = c(
        "Observed" = "grey75",
        "Censored" = "#E69F00"
      )
    ) +
    scale_color_manual(
      values = c(
        "UVN fit" = "black",
        "BVN fit" = "#009E73"
      )
    ) +
    labs(
      title = title_text,
      x = expression(log[10](DNA)),
      y = "Density",
      fill = NULL,
      color = NULL
    ) +
    theme_bw() +
    coord_cartesian(clip = "off")

  if (facet) {
    p <- p + facet_wrap(~ source, scales = "free", ncol = 2)
  }

  p
}

fit_results <- lapply(seq_len(nrow(source_spec)), function(i) {
  fit_source_model(source_spec[i, , drop = FALSE], n_draws = 5000)
})

observed_df <- bind_rows(lapply(fit_results, `[[`, "observed")) %>%
  mutate(source = factor(source, levels = source_spec$source))

fit_draws_df <- bind_rows(lapply(fit_results, `[[`, "fit_draws")) %>%
  mutate(source = factor(source, levels = source_spec$source))

bvn_fit_draws_df <- build_bvn_overlay_df(source_spec, n_draws = 5000)

if (nrow(bvn_fit_draws_df) > 0) {
  bvn_fit_draws_df <- bvn_fit_draws_df %>%
    mutate(source = factor(source, levels = source_spec$source))
}

curve_draws_df <- bind_rows(fit_draws_df, bvn_fit_draws_df)

fit_summary_df <- bind_rows(lapply(fit_results, `[[`, "fit_summary")) %>%
  mutate(source = factor(source, levels = source_spec$source)) %>%
  arrange(source)

lod_max_df <- observed_df %>%
  filter(cens == 1L, !is.na(lod), is.finite(lod)) %>%
  group_by(source, source_label) %>%
  summarise(
    max_lod = max(lod, na.rm = TRUE),
    .groups = "drop"
  )

hist_df <- make_hist_df(observed_df, n_bins = 20, min_binwidth = 0.25)

overview_plot <- make_source_plot(
  hist_data = hist_df,
  fit_data = curve_draws_df,
  lod_data = lod_max_df,
  title_text = "fitdistrplus left-censored normal fits with EB/HH BVN overlay",
  facet = TRUE
)

print(overview_plot)

ggsave(
  filename = file.path(getwd(), "fitdistrplus_leftcens_overview.png"),
  plot = overview_plot,
  width = 8,
  height = 6,
  dpi = 300
)

for (src in source_spec$source) {
  src_label <- source_spec$source_label[source_spec$source == src][1]

  source_plot <- make_source_plot(
    hist_data = hist_df %>% filter(as.character(source) == src),
    fit_data = curve_draws_df %>% filter(as.character(source) == src),
    lod_data = lod_max_df %>% filter(as.character(source) == src),
    title_text = paste0("fitdistrplus left-censored normal: ", src_label, " (", src, ")"),
    facet = FALSE
  )

  print(source_plot)

  ggsave(
    filename = file.path(getwd(), paste0("fitdistrplus_leftcens_", src, ".png")),
    plot = source_plot,
    width = 6,
    height = 4.5,
    dpi = 300
  )
}

write.csv(
  fit_summary_df,
  file = file.path(getwd(), "fitdistrplus_leftcens_summary.csv"),
  row.names = FALSE
)

fit_object_list <- setNames(
  lapply(fit_results, `[[`, "fit_object"),
  source_spec$source
)

saveRDS(
  fit_object_list,
  file = file.path(getwd(), "fitdistrplus_leftcens_fit_objects.rds")
)

print(fit_summary_df)

