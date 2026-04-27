# clean_behav.R
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(lubridate)
})

# ---------------------------
# Helpers
# ---------------------------
normalize_missing_chr <- function(x,
                                  missing_tokens = c(
                                    "", "na", "n/a", "null", ".", "nan",
                                    "unknown", "unk", "missing"
                                  )) {
  # Trim + lowercase for token matching, but keep original case for non-missing values
  x_trim <- str_trim(x)
  # Convert "" to NA
  x_trim[x_trim == ""] <- NA_character_
  
  # Token-based NA
  # Match case-insensitively by lowering a copy
  low <- tolower(x_trim)
  low[is.na(low)] <- NA_character_
  is_miss <- !is.na(low) & low %in% missing_tokens
  x_trim[is_miss] <- NA_character_
  
  x_trim
}

coerce_datetime <- function(x, tz = "UTC") {
  # Accept POSIXct already
  if (inherits(x, "POSIXct")) return(x)
  
  # Try a few common formats
  # If x is numeric (Excel date), let user pre-handle; here treat as NA
  if (is.numeric(x)) return(as.POSIXct(NA))
  
  x <- as.character(x)
  x <- str_trim(x)
  x[x == ""] <- NA_character_
  
  parsed <- suppressWarnings(ymd_hms(x, tz = tz))
  if (all(is.na(parsed))) parsed <- suppressWarnings(mdy_hms(x, tz = tz))
  if (all(is.na(parsed))) parsed <- suppressWarnings(ymd_hm(x, tz = tz))
  if (all(is.na(parsed))) parsed <- suppressWarnings(mdy_hm(x, tz = tz))
  parsed
}

extract_obs_id_from_name <- function(name_str) {
  # Mimic repo logic: split on - or . and take 2nd & 3rd tokens
  # Example: 20210526–Ballo–003–Name fixed from 013  -> Ballo_003
  tokens <- strsplit(name_str, "[-.]+")[[1]]
  tokens <- tokens[!is.na(tokens) & tokens != ""]
  if (length(tokens) >= 3) {
    paste(tokens[2], tokens[3], sep = "_")
  } else {
    NA_character_
  }
}

validate_allowed_category <- function(category,
                                      allowed = c("Location","Compartment","Activity","Mouthing","Touching")) {
  category %in% allowed
}

# ---------------------------
# Main cleaner
# ---------------------------
clean_behav_dataset <- function(df,
                                tz = "UTC",
                                allowed_categories = c("Location","Compartment","Activity","Mouthing","Touching"),
                                state_categories = c("Location","Compartment","Activity"),
                                event_categories = c("Mouthing","Touching"),
                                missing_tokens = c("", "na", "n/a", "null", ".", "nan", "unknown", "unk", "missing"),
                                duration_unit = c("seconds","minutes"),
                                recompute_duration_from_datetime = TRUE,
                                drop_negative_or_zero_duration = TRUE,
                                create_child_id_with_hh = TRUE,
                                strict_time_order_within_child = TRUE,
                                keep_audit_rows = TRUE) {
  
  duration_unit <- match.arg(duration_unit)
  
  # --- Audit table for removed rows ---
  audit <- NULL
  if (keep_audit_rows) {
    audit <- df[0, , drop = FALSE]
    audit$.__drop_reason__ <- character(0)
  }
  
  add_audit <- function(rows, reason) {
    if (!keep_audit_rows || nrow(rows) == 0) return(invisible(NULL))
    rows$.__drop_reason__ <- reason
    audit <<- bind_rows(audit, rows)
    invisible(NULL)
  }
  
  # --- 0) Standardize column names (do not rename user data, just map known variants) ---
  # Expected columns in repo-style data:
  # name, category, item, Start.datetime, End.datetime, Duration, hh_id
  # Handle minor variants in case user has Start.da/End.da etc.
  colmap <- names(df)
  
  # Create working copy
  x <- df
  
  # --- 1) Normalize character missingness for key character columns if they exist ---
  for (nm in intersect(names(x), c("name","Name","category","item"))) {
    if (is.character(x[[nm]]) || is.factor(x[[nm]])) {
      x[[nm]] <- normalize_missing_chr(as.character(x[[nm]]), missing_tokens = missing_tokens)
    }
  }
  
  # --- 2) Parse datetimes if present ---
  # Try common repo columns:
  if ("Start.datetime" %in% names(x)) x$Start.datetime <- coerce_datetime(x$Start.datetime, tz = tz)
  if ("End.datetime"   %in% names(x)) x$End.datetime   <- coerce_datetime(x$End.datetime, tz = tz)
  
  # Some datasets use Start.da/End.da + Start.ti/Ending.t; you can extend here if needed.
  # We won't guess those automatically to avoid silent errors.
  
  # --- 3) Construct obs_id from 'name' (lowercase) when possible ---
  if ("name" %in% names(x)) {
    x$obs_id <- vapply(x$name, function(s) {
      if (is.na(s)) return(NA_character_)
      extract_obs_id_from_name(s)
    }, character(1))
  } else {
    x$obs_id <- NA_character_
  }
  
  # --- 4) Optional: child_id = hh_id + obs_id (safer uniqueness) ---
  if (create_child_id_with_hh && all(c("hh_id","obs_id") %in% names(x))) {
    x$child_id <- paste0(x$hh_id, "__", x$obs_id)
  } else if ("obs_id" %in% names(x)) {
    x$child_id <- x$obs_id
  } else {
    x$child_id <- NA_character_
  }
  
  # --- 5) Basic required fields for any usable row ---
  # We DO NOT drop on every NA in the dataset; only on fields needed for that row type.
  # a) category must be known for both state & event rows
  if ("category" %in% names(x)) {
    bad_cat <- is.na(x$category) | !validate_allowed_category(x$category, allowed = allowed_categories)
  } else {
    bad_cat <- rep(TRUE, nrow(x))
  }
  
  if (any(bad_cat, na.rm = TRUE)) {
    add_audit(x[bad_cat, , drop = FALSE], "drop: missing/invalid category")
    x <- x[!bad_cat, , drop = FALSE]
  }
  
  # b) name / child_id: for modeling we need to group by subject
  # Use child_id; if missing, drop
  bad_child <- is.na(x$child_id) | x$child_id == ""
  if (any(bad_child)) {
    add_audit(x[bad_child, , drop = FALSE], "drop: missing child_id (cannot group rows)")
    x <- x[!bad_child, , drop = FALSE]
  }
  
  # c) item needed for both state & event; but treat differently:
  # - state rows must have item
  # - event rows must have item too (event type), otherwise meaningless
  bad_item <- is.na(x$item) | x$item == ""
  if (any(bad_item)) {
    add_audit(x[bad_item, , drop = FALSE], "drop: missing item")
    x <- x[!bad_item, , drop = FALSE]
  }
  
  # --- 6) Handle time + duration ---
  # Strategy:
  # - If Start/End exist and recompute is TRUE: recompute Duration from them
  # - Else if Duration exists: keep it but check validity
  # - If neither: drop (cannot model time)
  has_dt <- all(c("Start.datetime","End.datetime") %in% names(x))
  has_dur <- "Duration" %in% names(x)
  
  if (has_dt && recompute_duration_from_datetime) {
    # recompute in seconds
    dur_sec <- as.numeric(difftime(x$End.datetime, x$Start.datetime, units = "secs"))
    x$Duration_sec <- dur_sec
  } else if (has_dur) {
    # If duration is provided, store as seconds consistently
    # Assume user told us unit
    if (duration_unit == "seconds") {
      x$Duration_sec <- suppressWarnings(as.numeric(x$Duration))
    } else {
      x$Duration_sec <- suppressWarnings(as.numeric(x$Duration)) * 60
    }
  } else {
    add_audit(x, "drop: no datetime and no duration")
    x <- x[0, , drop = FALSE]
  }
  
  # Drop rows with missing time
  bad_time <- is.na(x$Duration_sec)
  if (has_dt) {
    bad_time <- bad_time | is.na(x$Start.datetime) | is.na(x$End.datetime)
  }
  if (any(bad_time)) {
    add_audit(x[bad_time, , drop = FALSE], "drop: missing datetime/duration")
    x <- x[!bad_time, , drop = FALSE]
  }
  
  # Drop nonpositive / negative duration if requested
  if (drop_negative_or_zero_duration) {
    bad_dur <- x$Duration_sec <= 0
    if (any(bad_dur)) {
      add_audit(x[bad_dur, , drop = FALSE], "drop: nonpositive duration")
      x <- x[!bad_dur, , drop = FALSE]
    }
  }
  
  # --- 7) Separate state vs event rows (for additional checks) ---
  x$row_type <- ifelse(x$category %in% state_categories, "state",
                       ifelse(x$category %in% event_categories, "event", "other"))
  
  # For "other" categories (if any), keep but flag; you can choose to drop them
  other_rows <- x$row_type == "other"
  if (any(other_rows)) {
    # conservative: drop unknown types
    add_audit(x[other_rows, , drop = FALSE], "drop: category not in state/event sets")
    x <- x[!other_rows, , drop = FALSE]
  }
  
  # --- 8) Enforce ordering within child (optional strict check) ---
  # We do NOT reorder silently if strict=TRUE; we will sort and then check monotonicity.
  if (strict_time_order_within_child && has_dt) {
    x <- x %>% arrange(child_id, Start.datetime, End.datetime)
    
    # Check monotonic Start times within each child
    bad_order <- x %>%
      group_by(child_id) %>%
      mutate(prev_end = lag(End.datetime),
             order_ok = is.na(prev_end) | Start.datetime >= prev_end) %>%
      ungroup() %>%
      mutate(bad = !order_ok)
    
    if (any(bad_order$bad, na.rm = TRUE)) {
      add_audit(bad_order[bad_order$bad, names(x), drop = FALSE],
                "drop: time overlap / non-monotone within child (strict)")
      x <- bad_order[!bad_order$bad, names(x), drop = FALSE]
    }
  } else {
    # still sort for consistency
    if (has_dt) x <- x %>% arrange(child_id, Start.datetime, End.datetime)
  }
  
  # --- 9) Build a report ---
  report <- list()
  
  report$n_raw <- nrow(df)
  report$n_clean <- nrow(x)
  report$n_dropped <- report$n_raw - report$n_clean
  
  if (keep_audit_rows) {
    report$drop_reasons <- audit %>%
      count(.__drop_reason__, sort = TRUE) %>%
      rename(reason = .__drop_reason__, n = n)
  }
  
  report$n_children <- length(unique(x$child_id))
  report$children_top_counts <- x %>% count(child_id, sort = TRUE) %>% head(10)
  
  if (has_dt) {
    report$time_span_by_child <- x %>%
      group_by(child_id) %>%
      summarise(
        start_min = min(Start.datetime),
        end_max = max(End.datetime),
        n_rows = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(n_rows))
  }
  
  # Return
  out <- list(cleaned = x, audit = audit, report = report)
  class(out) <- c("clean_behav_result", class(out))
  out
}

# ---------------------------
# Convenience printer
# ---------------------------
print.clean_behav_result <- function(x, ...) {
  cat("Clean behav dataset\n")
  cat("  raw rows   :", x$report$n_raw, "\n")
  cat("  clean rows :", x$report$n_clean, "\n")
  cat("  dropped    :", x$report$n_dropped, "\n")
  cat("  children   :", x$report$n_children, "\n")
  if (!is.null(x$report$drop_reasons)) {
    cat("\nTop drop reasons:\n")
    print(x$report$drop_reasons, row.names = FALSE)
  }
  invisible(x)
}