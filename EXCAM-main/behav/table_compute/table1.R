setwd("D:/1. Yixuan Work/Spring 2026/RA/Risk Assessment/EXCAM-main/behav")
old <- getwd()

setwd("data")
behav_t1 <- readRDS("observational_data_tp1_processed.RDS")
behav_t2 <- readRDS("observational_data_tp2_processed_update.RDS")

library(dplyr)
library(tidyr)
library(stringr)

`%||%` <- function(a, b) {
  if (length(a) == 0 || all(is.na(a))) return(b)
  a
}

# --------------------------------------------------
# 1. standardize category / item
# --------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)

`%||%` <- function(a, b) {
  if (length(a) == 0 || all(is.na(a))) return(b)
  a
}

# -----------------------------
# ID functions
# -----------------------------
extract.obs_id <- function(name) {
  paste(strsplit(as.character(name), "[-.]+")[[1]][2:3], collapse = "_")
}

extract.child_id <- function(name) {
  parts <- strsplit(as.character(name), "[-.]+")[[1]]
  third <- if (length(parts) >= 3) parts[3] else NA_character_
  m <- regmatches(third, regexpr("[0-9]{3}", third))
  ifelse(length(m) == 0 || is.na(m), NA_character_, m)
}

# -----------------------------
# item/category standardization
# for final Table 1 summaries
# -----------------------------
standardize_category <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  case_when(
    x == "activity"    ~ "activity",
    x == "compartment" ~ "compartment",
    x == "location"    ~ "location",
    x == "eating"      ~ "eating",
    x == "mouthing"    ~ "mouthing",
    x == "pica"        ~ "pica",
    x == "touching"    ~ "touching",
    x %in% c("mistake", "") ~ "mistake",
    TRUE ~ x
  )
}

standardize_item <- function(x) {
  x0 <- str_to_lower(str_trim(as.character(x)))
  case_when(
    # activity
    x0 == "awake" ~ "awake",
    x0 == "bathing" ~ "bathing",
    x0 == "sleeping" ~ "sleeping",
    x0 %in% c("drinking-breastmilk", "drinking-water", "drinking-other") ~ "drinking",
    
    # compartment
    x0 %in% c("carried-mom arms", "carried-mom back") ~ "carried_by_mother",
    x0 %in% c("carried-other arms", "carried-other back") ~ "carried_by_other",
    x0 == "down-bare ground" ~ "bare_ground",
    x0 == "down-with barrier" ~ "surface_with_barriers",
    
    # location
    x0 == "on homestead" ~ "within_homestead",
    x0 == "off homestead" ~ "out_of_homestead",
    
    TRUE ~ x0
  )
}

# -----------------------------
# simplified item names exactly
# for paper-style duration cleaning
# -----------------------------
simplify_item_paper <- function(x) {
  y <- as.character(x)
  y[y %in% c("carried-mom arms", "carried-mom back")] <- "carried-mom"
  y[y %in% c("carried-other arms", "carried-other back")] <- "carried-other"
  y[y == "On homestead"] <- "on homestead"
  y[y %in% c("drinking-breastmilk", "drinking-other", "drinking-water")] <- "drinking"
  y
}

# --------------------------------------------------
# 3. main preprocessing
# --------------------------------------------------
prep_behav <- function(df, tp = c("tp1", "tp2")) {
  tp <- match.arg(tp)
  
  x <- df
  
  x$obs_id <- sapply(x$name, extract.obs_id)
  x$child_id <- sapply(x$name, extract.child_id)
  
  # paper cleaning only fixes this in TP1
  if (tp == "tp1") {
    x$obs_id[x$obs_id == "meri_126_named corrected from 125"] <- "meri_126"
  }
  
  # paper uses Start.datetime / End.datetime and format %m/%d/%Y %H:%M:%S
  start_col <- if ("Start.datetime" %in% names(x)) "Start.datetime" else "Start.time"
  end_col   <- if ("End.datetime" %in% names(x)) "End.datetime" else "Ending.time"
  
  x$start <- as.POSIXct(strptime(as.character(x[[start_col]]), "%m/%d/%Y %H:%M:%S"))
  x$end   <- as.POSIXct(strptime(as.character(x[[end_col]]), "%m/%d/%Y %H:%M:%S"))
  
  x <- x %>%
    mutate(
      category_std = standardize_category(category),
      item_std = standardize_item(item),
      item_simple = simplify_item_paper(item),
      duration_sec2 = suppressWarnings(as.numeric(Duration)),
      duration_hr = duration_sec2 / 3600
    )
  
  x <- x[order(x$obs_id, x$start), , drop = FALSE]
  rownames(x) <- NULL
  x
}

# --------------------------------------------------
# 4. paper-style cleaned duration data
# --------------------------------------------------
build_duration_cleaned <- function(df, tp = c("tp1", "tp2"), verbose = FALSE) {
  tp <- match.arg(tp)
  
  x <- prep_behav(df, tp = tp)
  
  # paper starts from hh_obs %>% filter(Duration > 0)
  x <- x %>%
    filter(!is.na(duration_sec2), duration_sec2 > 0)
  
  # remove records with missing item
  x <- x %>%
    filter(!is.na(item), item != "")
  
  if (verbose) {
    cat("after Duration>0 and item!='':", nrow(x), "rows\n")
    cat("unique obs_id:", dplyr::n_distinct(x$obs_id), "\n")
  }
  
  # initialize state columns
  x$loc  <- NA_character_
  x$comp <- NA_character_
  x$act  <- NA_character_
  
  # fill loc/comp/act forward within obs_id
  for (i.id in unique(x$obs_id)) {
    temp.ind <- which(x$obs_id == i.id)
    temp.loc <- NA_character_
    temp.comp <- NA_character_
    temp.act <- NA_character_
    
    for (j in temp.ind) {
      if (x$category[j] == "Location") {
        temp.loc <- x$item_simple[j]
      }
      if (x$category[j] == "Compartment") {
        temp.comp <- x$item_simple[j]
      }
      if (x$category[j] == "Activity") {
        temp.act <- x$item_simple[j]
      }
      
      x$loc[j]  <- temp.loc
      x$comp[j] <- temp.comp
      x$act[j]  <- temp.act
    }
  }
  
  # remove initial incomplete rows
  x <- x %>%
    filter(!is.na(loc), !is.na(comp), !is.na(act))
  
  if (verbose) {
    cat("after removing incomplete states:", nrow(x), "rows\n")
  }
  
  if (nrow(x) == 0) {
    x$dur <- numeric(0)
    return(x)
  }
  
  x$loc_comp <- paste(x$loc, x$comp, sep = "_")
  x$state    <- paste(x$loc, x$comp, x$act, sep = "_")
  
  # remove consecutive duplicate state within obs_id, recompute dur each round
  repeat {
    if (nrow(x) <= 1) break
    
    dup_ind <- which(
      x$obs_id[-1] == x$obs_id[-nrow(x)] &
        x$state[-1]  == x$state[-nrow(x)]
    ) + 1
    
    if (length(dup_ind) == 0) break
    
    x <- x[-dup_ind, , drop = FALSE]
    rownames(x) <- NULL
    
    # recompute dur
    x$dur <- NA_real_
    for (i.id in unique(x$obs_id)) {
      temp.ind <- which(x$obs_id == i.id)
      if (length(temp.ind) == 0) next
      
      for (k in seq_along(temp.ind)) {
        j <- temp.ind[k]
        if (k == length(temp.ind)) {
          x$dur[j] <- as.numeric(difftime(x$end[j], x$start[j], units = "secs"))
        } else {
          j_next <- temp.ind[k + 1]
          x$dur[j] <- as.numeric(difftime(x$start[j_next], x$start[j], units = "secs"))
        }
      }
    }
    
    # remove zero-duration rows
    zero_ind <- which(!is.na(x$dur) & x$dur == 0)
    if (length(zero_ind) > 0) {
      x <- x[-zero_ind, , drop = FALSE]
      rownames(x) <- NULL
    }
    
    if (nrow(x) == 0) break
  }
  
  # if dur not yet computed, compute once
  if (nrow(x) > 0 && (!("dur" %in% names(x)) || all(is.na(x$dur)))) {
    x$dur <- NA_real_
    for (i.id in unique(x$obs_id)) {
      temp.ind <- which(x$obs_id == i.id)
      if (length(temp.ind) == 0) next
      
      for (k in seq_along(temp.ind)) {
        j <- temp.ind[k]
        if (k == length(temp.ind)) {
          x$dur[j] <- as.numeric(difftime(x$end[j], x$start[j], units = "secs"))
        } else {
          j_next <- temp.ind[k + 1]
          x$dur[j] <- as.numeric(difftime(x$start[j_next], x$start[j], units = "secs"))
        }
      }
    }
  }
  
  if (verbose) {
    cat("after duplicate-state cleaning:", nrow(x), "rows\n")
    cat("total cleaned hours:", sum(x$dur, na.rm = TRUE) / 3600, "\n")
  }
  
  x
}

# --------------------------------------------------
# 5. helper functions
# --------------------------------------------------
count_true_transitions <- function(df, category_name) {
  df %>%
    filter(category_std == category_name) %>%
    arrange(obs_id, start, end) %>%
    group_by(obs_id) %>%
    mutate(
      prev_item = lag(item_std),
      changed = !is.na(prev_item) & item_std != prev_item
    ) %>%
    ungroup() %>%
    summarise(n = sum(changed, na.rm = TRUE)) %>%
    pull(n)
}

average_pct_by_child <- function(df, category_name, item_levels) {
  df %>%
    filter(category_std == category_name, item_std %in% item_levels) %>%
    group_by(child_id, item_std) %>%
    summarise(time_hr = sum(duration_hr, na.rm = TRUE), .groups = "drop") %>%
    group_by(child_id) %>%
    mutate(prop = 100 * time_hr / sum(time_hr, na.rm = TRUE)) %>%
    ungroup() %>%
    complete(child_id, item_std = item_levels, fill = list(prop = 0)) %>%
    group_by(item_std) %>%
    summarise(avg_pct = mean(prop, na.rm = TRUE), .groups = "drop")
}

# --------------------------------------------------
# 6. Table 1 stats
# --------------------------------------------------
make_table1_stats <- function(df, tp = c("tp1", "tp2"), verbose = FALSE) {
  tp <- match.arg(tp)
  
  x <- prep_behav(df, tp = tp)
  
  x_use <- x %>%
    filter(category_std != "mistake") %>%
    filter(!is.na(item_std), item_std != "") %>%
    filter(!is.na(child_id))
  
  x_dur <- build_duration_cleaned(df, tp = tp, verbose = verbose)
  
  # A. subjects + age: use child_id
  child_age <- x_use %>%
    distinct(child_id, age_days) %>%
    filter(!is.na(age_days))
  
  n_subjects <- n_distinct(child_age$child_id)
  
  age_summary <- if (nrow(child_age) > 0) {
    sprintf(
      "%.0f (%.0f–%.0f)",
      mean(child_age$age_days, na.rm = TRUE),
      min(child_age$age_days, na.rm = TRUE),
      max(child_age$age_days, na.rm = TRUE)
    )
  } else {
    NA_character_
  }
  
  # B. observation hours: use cleaned recomputed dur
  total_obs_hours <- sum(x_dur$dur, na.rm = TRUE) / 3600
  
  # C. activity transitions
  n_activity_transitions <- count_true_transitions(x_use, "activity")
  
  # D. activity % by child
  activity_levels <- c("awake", "bathing", "drinking", "sleeping")
  activity_pct <- average_pct_by_child(x_use, "activity", activity_levels)
  
  # E. frequency events and rates
  freq_cats <- c("eating", "mouthing", "pica", "touching")
  
  freq_event_total <- x_use %>%
    filter(category_std %in% freq_cats) %>%
    nrow()
  
  event_rate <- x_use %>%
    filter(category_std %in% freq_cats) %>%
    count(category_std, name = "n_events") %>%
    mutate(rate_per_hour = n_events / total_obs_hours)
  
  event_rate <- tibble(category_std = freq_cats) %>%
    left_join(event_rate, by = "category_std") %>%
    mutate(
      n_events = replace_na(n_events, 0),
      rate_per_hour = replace_na(rate_per_hour, 0)
    )
  
  # F. compartment
  n_compartment_transitions <- count_true_transitions(x_use, "compartment")
  
  compartment_levels <- c(
    "carried_by_mother",
    "carried_by_other",
    "bare_ground",
    "surface_with_barriers"
  )
  compartment_pct <- average_pct_by_child(x_use, "compartment", compartment_levels)
  
  # G. location
  n_location_transitions <- count_true_transitions(x_use, "location")
  
  location_levels <- c("out_of_homestead", "within_homestead")
  location_pct <- average_pct_by_child(x_use, "location", location_levels)
  
  tibble(
    metric = c(
      "# subjects",
      "mean age (range) in days",
      "duration of observation (hours)",
      "# duration-based activity transitions",
      "average % of time spent awake",
      "average % of time spent bathing",
      "average % of time spent drinking",
      "average % of time spent sleeping",
      "# frequency-based activities",
      "rate of eating (times per hour)",
      "rate of mouthing (times per hour)",
      "rate of pica (times per hour)",
      "rate of touching (times per hour)",
      "# compartment transitions",
      "average % of time spent carried by mother",
      "average % of time spent carried by other",
      "average % of time spent on the bare ground",
      "average % of time spent on a surface with barriers",
      "# location transitions",
      "average % of time spent out of homestead",
      "average % of time spent within homestead"
    ),
    value = c(
      as.character(n_subjects),
      age_summary,
      sprintf("%.0f", total_obs_hours),
      sprintf("%d", n_activity_transitions),
      sprintf("%.1f", activity_pct$avg_pct[match("awake", activity_pct$item_std)] %||% 0),
      sprintf("%.1f", activity_pct$avg_pct[match("bathing", activity_pct$item_std)] %||% 0),
      sprintf("%.1f", activity_pct$avg_pct[match("drinking", activity_pct$item_std)] %||% 0),
      sprintf("%.1f", activity_pct$avg_pct[match("sleeping", activity_pct$item_std)] %||% 0),
      sprintf("%d", freq_event_total),
      sprintf("%.2f", event_rate$rate_per_hour[match("eating", event_rate$category_std)] %||% 0),
      sprintf("%.2f", event_rate$rate_per_hour[match("mouthing", event_rate$category_std)] %||% 0),
      sprintf("%.2f", event_rate$rate_per_hour[match("pica", event_rate$category_std)] %||% 0),
      sprintf("%.2f", event_rate$rate_per_hour[match("touching", event_rate$category_std)] %||% 0),
      sprintf("%d", n_compartment_transitions),
      sprintf("%.1f", compartment_pct$avg_pct[match("carried_by_mother", compartment_pct$item_std)] %||% 0),
      sprintf("%.1f", compartment_pct$avg_pct[match("carried_by_other", compartment_pct$item_std)] %||% 0),
      sprintf("%.1f", compartment_pct$avg_pct[match("bare_ground", compartment_pct$item_std)] %||% 0),
      sprintf("%.1f", compartment_pct$avg_pct[match("surface_with_barriers", compartment_pct$item_std)] %||% 0),
      sprintf("%d", n_location_transitions),
      sprintf("%.1f", location_pct$avg_pct[match("out_of_homestead", location_pct$item_std)] %||% 0),
      sprintf("%.1f", location_pct$avg_pct[match("within_homestead", location_pct$item_std)] %||% 0)
    )
  )
}

# --------------------------------------------------
# 7. run
# --------------------------------------------------
tab1_t1 <- make_table1_stats(behav_t1, tp = "tp1")
tab1_t2 <- make_table1_stats(behav_t2, tp = "tp2")

table1_result <- tab1_t1 %>%
  rename(timepoint1 = value) %>%
  left_join(tab1_t2 %>% rename(timepoint2 = value), by = "metric")

print(table1_result, n = Inf)
write.table(
  table1_result,
  "table1_result.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)