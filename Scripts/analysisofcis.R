# ==============================================================================
# STANDALONE MONTHLY AUTOCORRELATION & INDEPENDENCE AUDIT
# Companion Verification Script for: Virginia Traffic Safety Executive Dashboard
# Tests the independence assumption behind all monthly-grouped CIs (Pages 1 & 2)
# ==============================================================================
# Required packages (all already used by app.R):
# install.packages(c("tidyverse", "lubridate", "scales", "writexl"))

library(tidyverse)
library(lubridate)
library(scales)
library(writexl)

# ==============================================================================
# 1. REPRODUCIBLE DATA LOADING & PREPROCESSING (IDENTICAL TO DASHBOARD)
# ==============================================================================
data_paths <- c(
  "../Data/Virginia_Accidents_With_Pop.csv",
  "Data/Virginia_Accidents_With_Pop.csv",
  "Virginia_Accidents_With_Pop.csv"
)

existing_path <- data_paths[file.exists(data_paths)][1]

if (is.na(existing_path)) {
  stop(
    "Data file 'Virginia_Accidents_With_Pop.csv' not found. ",
    "Ensure it is located in '../Data/' or 'Data/' relative to this script."
  )
}

cat("Loading dashboard source data from:", existing_path, "\n")
acc_raw <- read.csv(existing_path, na.strings = c("", "NA"), stringsAsFactors = FALSE)

classify_weather_simple <- function(w) {
  w_low <- str_to_lower(coalesce(w, ""))
  case_when(
    w_low == "" ~ NA_character_,
    str_detect(w_low, "t-storm|thunder|heavy rain|squall|tornado|hail") ~ "Thunderstorms / Heavy Rain",
    str_detect(w_low, "snow|sleet|ice|freezing|wintry|blizzard|fog|mist|haze") ~ "Snow / Ice / Fog",
    str_detect(w_low, "rain|drizzle|shower|precip") ~ "Light-Moderate Rain / Drizzle",
    str_detect(w_low, "cloud|overcast") ~ "Cloudy / Overcast",
    str_detect(w_low, "clear|fair") ~ "Clear / Fair",
    TRUE ~ "Other Weather"
  )
}

acc <- acc_raw %>%
  mutate(
    Start_Time        = ymd_hms(Start_Time, tz = "UTC", quiet = TRUE),
    End_Time          = ymd_hms(End_Time, tz = "UTC", quiet = TRUE),
    Accident_Date     = as.Date(Start_Time),
    Year              = year(Start_Time),
    month_year        = as.Date(floor_date(Start_Time, unit = "month")),
    severe            = Severity >= 3,
    GEOID             = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    Weather_Condition = str_squish(Weather_Condition),
    Weather_Category  = classify_weather_simple(Weather_Condition),
    census_name       = str_squish(census_name)
  ) %>%
  filter(!is.na(census_name), !is.na(Severity), !is.na(Year), !is.na(month_year))

# Replicate dashboard population tiers
county_pop_ranks <- acc %>%
  group_by(GEOID, census_name) %>%
  summarise(
    population       = first(na.omit(population)),
    total_accidents  = n(),
    severe_accidents = sum(severe, na.rm = TRUE),
    severe_rate      = ifelse(total_accidents > 0, severe_accidents / total_accidents, 0),
    severe_ratio     = (severe_accidents / population) * 100000,
    .groups = "drop"
  ) %>%
  arrange(desc(population)) %>%
  mutate(pop_rank = row_number())

n_localities <- nrow(county_pop_ranks)
mid_start    <- max(1, floor((n_localities - 5) / 2) + 1)
mid_indices  <- mid_start:min(n_localities, mid_start + 4)

top5_pop_names    <- county_pop_ranks %>% slice_head(n = 5) %>% pull(census_name)
top10_pop_names   <- county_pop_ranks %>% slice_head(n = 10) %>% pull(census_name)
middle5_pop_names <- county_pop_ranks %>% slice(mid_indices) %>% pull(census_name)
bottom5_pop_names <- county_pop_ranks %>% slice_tail(n = 5) %>% pull(census_name)
all_counties      <- county_pop_ranks$census_name
all_months_global <- seq(min(acc$month_year), max(acc$month_year), by = "1 month")

# Identify the Top 15 localities shown in Page 1's "All Virginia" default charts
top15_by_ratio <- county_pop_ranks %>% slice_max(severe_ratio, n = 15) %>% pull(census_name)
top15_by_rate  <- county_pop_ranks %>% slice_max(severe_rate, n = 15) %>% pull(census_name)
top15_by_count <- county_pop_ranks %>% slice_max(total_accidents, n = 15) %>% pull(census_name)

county_meta <- county_pop_ranks %>%
  mutate(
    Pop_Tier = case_when(
      census_name %in% top5_pop_names    ~ "Top 5 Population",
      census_name %in% top10_pop_names   ~ "Top 6-10 Population",
      census_name %in% middle5_pop_names ~ "Middle 5 Population",
      census_name %in% bottom5_pop_names ~ "Bottom 5 Population",
      TRUE                               ~ "Other Locality"
    ),
    In_P1_Top15_Ratio = census_name %in% top15_by_ratio,
    In_P1_Top15_Rate  = census_name %in% top15_by_rate,
    In_P1_Top15_Count = census_name %in% top15_by_count
  ) %>%
  select(census_name, pop_rank, population, Pop_Tier, starts_with("In_P1_Top15_"))

# ==============================================================================
# 2. AUTOCORRELATION & CI-IMPACT DIAGNOSTIC FUNCTIONS
# ==============================================================================
check_series_independence <- function(dates, values, alpha = 0.05,
                                      min_months = 10, max_lag = 6) {
  valid_idx <- !is.na(dates)
  dates  <- as.Date(dates[valid_idx])
  values <- as.numeric(values[valid_idx])
  
  empty_out <- function(n_cal = 0L, n_obs = 0L, n_adj = 0L, note = "") {
    tibble(
      Calendar_Months      = as.integer(n_cal),
      Observed_Months      = as.integer(n_obs),
      Adjacent_Month_Pairs = as.integer(n_adj),
      Naive_Mean           = if (n_obs > 0) round(mean(values, na.rm = TRUE), 4) else NA_real_,
      Naive_SE             = if (n_obs > 1) round(sd(values, na.rm = TRUE) / sqrt(n_obs), 4) else NA_real_,
      Lag1_ACF             = NA_real_,
      Lag12_ACF            = NA_real_,
      LB_Lag1_Stat         = NA_real_,
      LB_Lag1_p            = NA_real_,
      Multi_Lag_Tested     = NA_integer_,
      LB_Multi_Stat        = NA_real_,
      LB_Multi_p           = NA_real_,
      AR1_SE_Multiplier    = NA_real_,
      AR1_Adjusted_SE      = NA_real_,
      Effective_N_AR1      = NA_real_,
      Status               = "REVIEW",
      Notes                = note
    )
  }
  
  if (length(dates) == 0) {
    return(empty_out(note = "No valid monthly dates available"))
  }
  
  # Reconstruct complete calendar-month grid so Lag 1 = strictly adjacent months
  full_months <- seq(min(dates), max(dates), by = "1 month")
  grid <- tibble(m = full_months) %>%
    left_join(
      tibble(m = dates, val = values) %>%
        group_by(m) %>%
        summarise(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
        mutate(val = ifelse(is.nan(val), NA_real_, val)),
      by = "m"
    )
  
  x     <- grid$val
  n_cal <- length(x)
  n_obs <- sum(!is.na(x))
  n_adj <- if (n_cal >= 2) sum(!is.na(x[-1]) & !is.na(x[-n_cal])) else 0L
  x_sd  <- sd(x, na.rm = TRUE)
  
  if (n_obs < min_months || n_adj < 6 || is.na(x_sd) || x_sd == 0) {
    return(empty_out(
      n_cal = n_cal,
      n_obs = n_obs,
      n_adj = n_adj,
      note  = paste0(
        "Insufficient data (<", min_months,
        " observed months or <6 adjacent pairs) or zero variance"
      )
    ))
  }
  
  naive_mean <- mean(x, na.rm = TRUE)
  naive_se   <- x_sd / sqrt(n_obs)
  
  lag_multi  <- as.integer(min(max_lag, max(1L, floor(n_obs / 3))))
  max_needed <- if (n_cal >= 24) max(lag_multi, 12L) else lag_multi
  
  acf_obj <- tryCatch(
    acf(x, lag.max = max_needed, plot = FALSE, na.action = na.pass),
    error = function(e) NULL
  )
  acf_vec <- if (!is.null(acf_obj)) as.numeric(acf_obj$acf)[-1] else numeric(0)
  
  lag1_r  <- if (length(acf_vec) >= 1) acf_vec[1] else NA_real_
  lag12_r <- if (n_cal >= 24 && length(acf_vec) >= 12) acf_vec[12] else NA_real_
  
  lb1      <- tryCatch(Box.test(x, lag = 1, type = "Ljung-Box"), error = function(e) NULL)
  lb1_stat <- if (!is.null(lb1)) unname(as.numeric(lb1$statistic)) else NA_real_
  lb1_p    <- if (!is.null(lb1)) unname(as.numeric(lb1$p.value)) else NA_real_
  
  lb_multi_obj <- if (
    lag_multi >= 2 &&
    length(acf_vec) >= lag_multi &&
    all(!is.na(acf_vec[1:lag_multi]))
  ) {
    tryCatch(Box.test(x, lag = lag_multi, type = "Ljung-Box"), error = function(e) NULL)
  } else {
    NULL
  }
  lb_multi_stat <- if (!is.null(lb_multi_obj)) unname(as.numeric(lb_multi_obj$statistic)) else NA_real_
  lb_multi_p    <- if (!is.null(lb_multi_obj)) unname(as.numeric(lb_multi_obj$p.value)) else NA_real_
  
  # Approximate AR(1) variance inflation for positive lag-1 autocorrelation:
  # Var(x_bar) ~ (sigma^2 / n) * ((1 + r1) / (1 - r1))
  se_mult <- if (!is.na(lag1_r) && lag1_r > 0) {
    r_cap <- min(lag1_r, 0.95)
    sqrt((1 + r_cap) / (1 - r_cap))
  } else if (!is.na(lag1_r)) {
    1.00
  } else {
    NA_real_
  }
  
  adj_se <- if (!is.na(se_mult)) naive_se * se_mult else NA_real_
  eff_n  <- if (!is.na(se_mult) && se_mult > 0) n_obs / (se_mult^2) else NA_real_
  
  sig_lag1  <- !is.na(lb1_p) && lb1_p < alpha
  sig_multi <- !is.na(lb_multi_p) && lb_multi_p < alpha
  
  status <- if (is.na(lb1_p)) {
    "REVIEW"
  } else if (sig_lag1 || sig_multi) {
    "FAIL"
  } else {
    "PASS"
  }
  
  notes <- if (status == "REVIEW") {
    "Ljung-Box test could not be evaluated due to missing lag pairs"
  } else if (sig_lag1 && !is.na(lag1_r) && lag1_r > 0) {
    paste0(
      "Positive Lag-1 autocorrelation (r1 = ", round(lag1_r, 2),
      "); naive monthly t-CI too narrow (~", round(se_mult, 2),
      "x SE; effective n ~ ", round(eff_n, 1), " of ", n_obs, " months)"
    )
  } else if (sig_lag1) {
    paste0("Negative Lag-1 autocorrelation (r1 = ", round(lag1_r, 2), ")")
  } else if (sig_multi) {
    paste0("Multi-lag autocorrelation through Lag ", lag_multi, " (seasonal/trend persistence)")
  } else {
    paste0("No significant monthly autocorrelation detected (alpha = ", alpha, ")")
  }
  
  tibble(
    Calendar_Months      = as.integer(n_cal),
    Observed_Months      = as.integer(n_obs),
    Adjacent_Month_Pairs = as.integer(n_adj),
    Naive_Mean           = round(naive_mean, 4),
    Naive_SE             = round(naive_se, 4),
    Lag1_ACF             = round(lag1_r, 3),
    Lag12_ACF            = round(lag12_r, 3),
    LB_Lag1_Stat         = round(lb1_stat, 2),
    LB_Lag1_p            = round(lb1_p, 4),
    Multi_Lag_Tested     = if (is.na(lb_multi_p)) NA_integer_ else lag_multi,
    LB_Multi_Stat        = round(lb_multi_stat, 2),
    LB_Multi_p           = round(lb_multi_p, 4),
    AR1_SE_Multiplier    = round(se_mult, 2),
    AR1_Adjusted_SE      = round(adj_se, 4),
    Effective_N_AR1      = round(eff_n, 1),
    Status               = status,
    Notes                = notes
  )
}

audit_monthly_independence <- function(df, group_cols, value_col, metric_label,
                                       dashboard_target, time_col = "month_year",
                                       alpha = 0.05, min_months = 10, max_lag = 6) {
  if (nrow(df) == 0) return(tibble())
  
  df %>%
    mutate(across(all_of(group_cols), as.character)) %>%
    group_by(across(all_of(group_cols))) %>%
    group_modify(~ check_series_independence(
      dates      = .x[[time_col]],
      values     = .x[[value_col]],
      alpha      = alpha,
      min_months = min_months,
      max_lag    = max_lag
    )) %>%
    ungroup() %>%
    mutate(
      Dashboard_CI_Target = dashboard_target,
      Metric              = metric_label,
      .before             = 1
    )
}

# ==============================================================================
# 3. SELF-VERIFICATION (NEGATIVE & POSITIVE CONTROLS FOR THE AUDIT FUNCTION)
# ==============================================================================
set.seed(663)
sim_dates  <- seq(as.Date("2016-01-01"), by = "1 month", length.out = 84)
sim_iid    <- rnorm(84, mean = 20, sd = 5)
sim_ar1    <- as.numeric(arima.sim(model = list(ar = 0.70), n = 84, sd = 5)) + 20

check_iid <- check_series_independence(sim_dates, sim_iid)
check_ar1 <- check_series_independence(sim_dates, sim_ar1)

stopifnot(
  check_iid$Status == "PASS",
  check_ar1$Status == "FAIL",
  check_ar1$Lag1_ACF > 0.40,
  check_ar1$AR1_SE_Multiplier > 1.5
)

method_self_check <- bind_rows(
  check_iid %>% mutate(Control_Case = "Negative Control: IID White Noise (Expected PASS)", .before = 1),
  check_ar1 %>% mutate(Control_Case = "Positive Control: Simulated AR(1) phi=0.70 (Expected FAIL)", .before = 1)
)
cat("Self-check passed: IID white noise -> PASS; AR(1) phi=0.70 -> FAIL.\n")

# ==============================================================================
# 4. SUITE 1: PAGE 1, PAGE 2 & TOP-5 EXPORT — LOCALITY MONTHLY CIs (ALL 133 VA)
#    Mirrors p1_monthly_variance(), p2_ts_computed(), and top5_worst_excel_sheets()
# ==============================================================================
monthly_locality_df <- acc %>%
  group_by(census_name, month_year) %>%
  summarise(
    severe_accidents = sum(severe, na.rm = TRUE),
    accidents        = n(),
    .groups = "drop"
  ) %>%
  complete(
    census_name = all_counties,
    month_year  = all_months_global,
    fill = list(severe_accidents = 0L, accidents = 0L)
  ) %>%
  left_join(county_pop_ranks %>% select(census_name, population), by = "census_name") %>%
  mutate(
    severe_ratio = (severe_accidents / population) * 100000,
    severe_rate  = ifelse(accidents == 0, NA_real_, severe_accidents / accidents)
  )

suite1_locality_monthly <- bind_rows(
  audit_monthly_independence(
    df               = monthly_locality_df,
    group_cols       = "census_name",
    value_col        = "severe_ratio",
    metric_label     = "Monthly Severe Crashes per 100k",
    dashboard_target = "Page 1 Monthly Ratio t-CI / Page 2 Trend / Top-5 Export"
  ),
  audit_monthly_independence(
    df               = monthly_locality_df,
    group_cols       = "census_name",
    value_col        = "severe_rate",
    metric_label     = "Monthly Severe Crash Share (%)",
    dashboard_target = "Page 1 Monthly Rate t-CI / Page 2 Trend / Top-5 Export"
  ),
  audit_monthly_independence(
    df               = monthly_locality_df,
    group_cols       = "census_name",
    value_col        = "severe_accidents",
    metric_label     = "Monthly Severe Crash Count",
    dashboard_target = "Page 2 Single-Locality Trend (Poisson Band)"
  )
) %>%
  left_join(county_meta, by = "census_name") %>%
  rename(Locality = census_name) %>%
  relocate(Pop_Tier, pop_rank, population, In_P1_Top15_Ratio, In_P1_Top15_Rate, In_P1_Top15_Count, .after = Locality) %>%
  arrange(Metric, pop_rank)

# ==============================================================================
# 5. SUITE 2: PAGE 2 MONTHLY WEATHER CIs (LOCALITY x 5 CORE WEATHER CATEGORIES)
#    Mirrors p2_qualified_monthly_weather() & p2_ci_data() rules exactly
# ==============================================================================
monthly_weather_all <- acc %>%
  filter(!is.na(Weather_Category), Weather_Category != "") %>%
  group_by(census_name, Weather_Group = Weather_Category, month_year) %>%
  summarise(
    severe_accidents = sum(severe, na.rm = TRUE),
    accidents        = n(),
    .groups = "drop"
  ) %>%
  left_join(county_pop_ranks %>% select(census_name, population), by = "census_name") %>%
  mutate(
    severe_rate  = severe_accidents / accidents,
    severe_ratio = (severe_accidents / population) * 100000
  )

# Apply exact Page 2 per-locality qualification fallback (>=5 crashes/month -> >=2 -> all)
qualified_monthly_weather <- map_dfr(all_counties, function(loc) {
  sub  <- monthly_weather_all %>% filter(census_name == loc)
  qual <- sub %>% filter(accidents >= 5)
  if (nrow(qual) < 5) qual <- sub %>% filter(accidents >= 2)
  if (nrow(qual) < 3) qual <- sub
  qual
})

# Keep Locality x Weather combinations that meet Page 2's active CI display rule (>=10 months, else >=5, else >=3)
qualifying_pairs <- qualified_monthly_weather %>%
  count(census_name, Weather_Group, name = "qual_months") %>%
  group_by(census_name) %>%
  group_modify(~ {
    f <- .x %>% filter(qual_months >= 10)
    if (nrow(f) == 0) f <- .x %>% filter(qual_months >= 5)
    if (nrow(f) == 0) f <- .x %>% filter(qual_months >= 3)
    f
  }) %>%
  ungroup()

p2_active_monthly_weather <- qualified_monthly_weather %>%
  inner_join(qualifying_pairs, by = c("census_name", "Weather_Group"))

suite2_p2_weather_monthly <- bind_rows(
  audit_monthly_independence(
    df               = p2_active_monthly_weather,
    group_cols       = c("census_name", "Weather_Group"),
    value_col        = "severe_rate",
    metric_label     = "Monthly Weather Severe Share (%)",
    dashboard_target = "Page 2 Monthly Weather t-CI (Rate)"
  ),
  audit_monthly_independence(
    df               = p2_active_monthly_weather,
    group_cols       = c("census_name", "Weather_Group"),
    value_col        = "severe_ratio",
    metric_label     = "Monthly Weather Severe per 100k",
    dashboard_target = "Page 2 Monthly Weather t-CI (Ratio)"
  )
) %>%
  left_join(county_meta %>% select(census_name, Pop_Tier, pop_rank), by = "census_name") %>%
  rename(Locality = census_name, Core_Weather_Category = Weather_Group) %>%
  relocate(Pop_Tier, pop_rank, .after = Locality) %>%
  arrange(Metric, pop_rank, Core_Weather_Category)

# ==============================================================================
# 6. SUITE 3: PAGE 1 TOP-10 RAW WEATHER CONDITIONS — MONTHLY DRIFT CHECK
#    Checks whether the pooled Page 1 weather Binomial CIs mask monthly autocorrelation
# ==============================================================================
top10_raw_weather <- acc %>%
  filter(!is.na(Weather_Condition), Weather_Condition != "") %>%
  group_by(Weather_Condition) %>%
  summarise(
    total_accidents  = n(),
    severe_accidents = sum(severe, na.rm = TRUE),
    severe_percent   = severe_accidents / total_accidents,
    .groups = "drop"
  ) %>%
  filter(total_accidents >= 20) %>%
  slice_max(severe_percent, n = 10) %>%
  pull(Weather_Condition)

raw_weather_monthly <- acc %>%
  filter(Weather_Condition %in% top10_raw_weather) %>%
  group_by(Weather_Condition, month_year) %>%
  summarise(
    accidents        = n(),
    severe_accidents = sum(severe, na.rm = TRUE),
    severe_rate      = severe_accidents / accidents,
    .groups = "drop"
  )

suite3_p1_raw_weather <- audit_monthly_independence(
  df               = raw_weather_monthly,
  group_cols       = "Weather_Condition",
  value_col        = "severe_rate",
  metric_label     = "Statewide Monthly Severe Share (%) — Top 10 Raw Weather",
  dashboard_target = "Page 1 Weather Binomial CI (Monthly Stability Check)"
) %>%
  arrange(desc(Lag1_ACF))

# ==============================================================================
# 7. SUITE 4: PAGE 1 SINGLE-YEAR FILTER SENSITIVITY CHECK (TOP 10 LOCALITIES)
#    Checks if monthly autocorrelation persists within individual 12-month years
# ==============================================================================
single_year_df <- monthly_locality_df %>%
  filter(census_name %in% top10_pop_names) %>%
  mutate(Year = year(month_year))

suite4_single_year <- bind_rows(
  audit_monthly_independence(
    df               = single_year_df,
    group_cols       = c("Year", "census_name"),
    value_col        = "severe_ratio",
    metric_label     = "Single-Year Monthly Severe per 100k",
    dashboard_target = "Page 1 Single-Year Monthly Ratio t-CI",
    min_months       = 10,
    max_lag          = 3
  ),
  audit_monthly_independence(
    df               = single_year_df,
    group_cols       = c("Year", "census_name"),
    value_col        = "severe_rate",
    metric_label     = "Single-Year Monthly Severe Share (%)",
    dashboard_target = "Page 1 Single-Year Monthly Rate t-CI",
    min_months       = 10,
    max_lag          = 3
  )
) %>%
  rename(Locality = census_name) %>%
  arrange(Metric, Year, Locality)

# ==============================================================================
# 8. EXECUTIVE ROLL-UP SUMMARY TABLE
# ==============================================================================
summarize_suite <- function(df, suite_name) {
  df %>%
    group_by(Dashboard_CI_Target, Metric) %>%
    summarise(
      Series_Evaluated   = n(),
      PASS_Count         = sum(Status == "PASS"),
      FAIL_Count         = sum(Status == "FAIL"),
      REVIEW_Count       = sum(Status == "REVIEW"),
      Pct_FAIL_of_Tested = ifelse(
        (PASS_Count + FAIL_Count) > 0,
        round(100 * FAIL_Count / (PASS_Count + FAIL_Count), 1),
        NA_real_
      ),
      Median_Lag1_ACF    = round(median(Lag1_ACF, na.rm = TRUE), 3),
      Max_Lag1_ACF       = if (all(is.na(Lag1_ACF))) NA_real_ else round(max(Lag1_ACF, na.rm = TRUE), 3),
      Median_SE_Mult     = round(median(AR1_SE_Multiplier, na.rm = TRUE), 2),
      Max_SE_Mult        = if (all(is.na(AR1_SE_Multiplier))) NA_real_ else round(max(AR1_SE_Multiplier, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    mutate(Audit_Suite = suite_name, .before = 1)
}

executive_summary <- bind_rows(
  summarize_suite(suite1_locality_monthly, "1. All 133 VA Localities (2016-2023)"),
  summarize_suite(
    suite1_locality_monthly %>% filter(Pop_Tier == "Top 5 Population"),
    "1b. Top 5 Population Localities Only"
  ),
  summarize_suite(suite2_p2_weather_monthly, "2. Page 2 Locality x Core Weather"),
  summarize_suite(suite3_p1_raw_weather, "3. Page 1 Top 10 Raw Weather Conditions"),
  summarize_suite(suite4_single_year, "4. Page 1 Single-Year Filter (Top 10 Localities)")
)

# ==============================================================================
# 9. PRINT CONSOLE SUMMARY
# ==============================================================================
cat("\n==============================================================================\n")
cat("MONTHLY INDEPENDENCE AUDIT — EXECUTIVE SUMMARY (Ljung-Box, alpha = 0.05)\n")
cat("==============================================================================\n")
print(executive_summary %>% select(Audit_Suite, Metric, PASS_Count, FAIL_Count, REVIEW_Count, Pct_FAIL_of_Tested, Median_Lag1_ACF, Median_SE_Mult), n = 30)

cat("\n--- Top 5 Most Populous VA Localities (Page 1 & Top-5 Export Monthly CIs) ---\n")
print(
  suite1_locality_monthly %>%
    filter(Pop_Tier == "Top 5 Population", Metric != "Monthly Severe Crash Count") %>%
    select(Metric, Locality, Observed_Months, Lag1_ACF, Lag12_ACF, LB_Lag1_p, LB_Multi_p, AR1_SE_Multiplier, Effective_N_AR1, Status),
  n = 20
)

cat("\n--- Default Page 2 Pair (Fairfax & Arlington) x Core Weather Categories ---\n")
print(
  suite2_p2_weather_monthly %>%
    filter(Locality %in% c("Fairfax County", "Arlington County")) %>%
    select(Metric, Locality, Core_Weather_Category, Observed_Months, Lag1_ACF, LB_Lag1_p, AR1_SE_Multiplier, Status),
  n = 30
)

# ==============================================================================
# 10. EXPORT MULTI-SHEET EXCEL AUDIT WORKBOOK
# ==============================================================================
out_xlsx <- paste0("VA_Monthly_Autocorrelation_Audit_", Sys.Date(), ".xlsx")
writexl::write_xlsx(
  list(
    Executive_Summary           = executive_summary,
    P1_P2_Locality_Monthly      = suite1_locality_monthly,
    P2_Locality_Weather_Monthly = suite2_p2_weather_monthly,
    P1_Raw_Weather_Monthly      = suite3_p1_raw_weather,
    P1_Single_Year_Top10        = suite4_single_year,
    Method_Self_Check           = method_self_check
  ),
  path = out_xlsx
)
cat("\nSaved full Excel audit report to:", normalizePath(out_xlsx, mustWork = FALSE), "\n")

# ==============================================================================
# 11. DIAGNOSTIC VISUALIZATIONS (DISPLAYED IN RSTUDIO & SAVED AS PNG)
# ==============================================================================
# Plot 1: Distribution of Lag-1 Autocorrelation across all 133 Virginia localities
approx_ci_bound <- 1.96 / sqrt(length(all_months_global))

p_lag1_dist <- suite1_locality_monthly %>%
  filter(!is.na(Lag1_ACF), Metric != "Monthly Severe Crash Count") %>%
  ggplot(aes(x = Lag1_ACF, fill = Status)) +
  geom_histogram(bins = 24, color = "white", alpha = 0.88) +
  geom_vline(xintercept = c(-approx_ci_bound, approx_ci_bound), linetype = "dashed", color = "#334155") +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("PASS" = "#0072B2", "FAIL" = "#9E1B32", "REVIEW" = "#94A3B8")) +
  labs(
    title    = "Monthly Lag-1 Autocorrelation Across Virginia Localities (2016–2023)",
    subtitle = paste0(
      "Dashed lines mark approximate ±1.96/sqrt(N) white-noise bounds (±",
      round(approx_ci_bound, 2), "); red bars fail Ljung-Box independence (alpha = 0.05)"
    ),
    x    = "Lag-1 Autocorrelation (r1 between consecutive calendar months)",
    y    = "Number of Virginia Localities",
    fill = "Ljung-Box Audit"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_lag1_dist)

# Plot 2: ACF Correlogram (Lags 1-12) for the Top 5 Most Populous VA Localities
top5_acf_df <- monthly_locality_df %>%
  filter(census_name %in% top5_pop_names) %>%
  pivot_longer(
    cols      = c(severe_ratio, severe_rate),
    names_to  = "Metric_Code",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = ifelse(
      Metric_Code == "severe_ratio",
      "Monthly Severe Crashes per 100k",
      "Monthly Severe Crash Share (%)"
    )
  ) %>%
  group_by(census_name, Metric) %>%
  arrange(month_year, .by_group = TRUE) %>%
  group_modify(~ {
    a <- acf(.x$Value, lag.max = 12, plot = FALSE, na.action = na.pass)
    tibble(
      Lag = 1:12,
      ACF = as.numeric(a$acf)[-1],
      CI  = 1.96 / sqrt(sum(!is.na(.x$Value)))
    )
  }) %>%
  ungroup()

p_top5_acf <- ggplot(top5_acf_df, aes(x = Lag, y = ACF)) +
  geom_hline(yintercept = 0, color = "#1A202C", linewidth = 0.4) +
  geom_hline(aes(yintercept = CI), linetype = "dashed", color = "#0072B2") +
  geom_hline(aes(yintercept = -CI), linetype = "dashed", color = "#0072B2") +
  geom_segment(aes(xend = Lag, yend = 0), color = "#0F2042", linewidth = 1.0) +
  geom_point(aes(color = abs(ACF) > CI), size = 2.2) +
  scale_color_manual(
    values = c("FALSE" = "#0F2042", "TRUE" = "#9E1B32"),
    labels = c("FALSE" = "Within white-noise bound", "TRUE" = "Exceeds 95% bound")
  ) +
  scale_x_continuous(breaks = 1:12) +
  facet_grid(census_name ~ Metric) +
  labs(
    title    = "Monthly Autocorrelation Function (Lags 1–12): Top 5 Virginia Localities",
    subtitle = "Red points indicate significant month-to-month persistence (Lag 1) or annual seasonality (Lag 12)",
    x        = "Lag (Months)",
    y        = "Autocorrelation (ACF)",
    color    = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

print(p_top5_acf)

ggsave("Audit_Plot1_Lag1_Distribution.png", p_lag1_dist, width = 9, height = 6.5, dpi = 300)
ggsave("Audit_Plot2_Top5_ACF_Correlogram.png", p_top5_acf, width = 10, height = 8.5, dpi = 300)
cat("Saved diagnostic plots to working directory.\n")