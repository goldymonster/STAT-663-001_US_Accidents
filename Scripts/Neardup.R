# ==============================================================================
# STANDALONE NEAR-DUPLICATE AUDIT (RSTUDIO CONSOLE ONLY — NO FILE EXPORTS)
# Companion Verification Script for: Virginia Traffic Safety Executive Dashboard
# Prints overall dataset duplicate %, year/source breakdowns, severity bias,
# Top-5 ranking stability, and Binomial CI impact directly in the R console.
# ==============================================================================
# Required packages (all already used by app.R):
# install.packages(c("tidyverse", "lubridate", "scales"))

library(tidyverse)
library(lubridate)
library(scales)

# ==============================================================================
# 1. LOAD & PREPROCESS DASHBOARD SOURCE DATA
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

if (!"ID" %in% names(acc_raw)) acc_raw$ID <- paste0("Row_", seq_len(nrow(acc_raw)))
if (!"Description" %in% names(acc_raw)) acc_raw$Description <- NA_character_

acc <- acc_raw %>%
  mutate(
    ID                = as.character(ID),
    Start_Time        = ymd_hms(Start_Time, tz = "UTC", quiet = TRUE),
    End_Time          = ymd_hms(End_Time, tz = "UTC", quiet = TRUE),
    Year              = year(Start_Time),
    severe            = Severity >= 3,
    GEOID             = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
    Weather_Condition = str_squish(Weather_Condition),
    Street            = str_squish(coalesce(Street, "")),
    census_name       = str_squish(census_name),
    Description       = str_squish(coalesce(as.character(Description), ""))
  ) %>%
  filter(!is.na(census_name), !is.na(Severity), !is.na(Year), !is.na(Start_Time))

county_pop_base <- acc %>%
  group_by(GEOID, census_name) %>%
  summarise(population = first(na.omit(population)), .groups = "drop")

# Dashboard CI & set-agreement helpers
get_adjusted_alpha <- function(m, method = c("bonferroni", "none"), alpha = 0.05) {
  method <- match.arg(method)
  m <- max(1, as.integer(m))
  switch(method, "bonferroni" = alpha / m, "none" = alpha)
}

calc_binom_ci <- function(x, n, bound = c("lower", "upper"), conf_level = 0.95) {
  bound <- match.arg(bound)
  idx <- ifelse(bound == "lower", 1, 2)
  conf_level <- pmin(pmax(as.numeric(conf_level)[1], 0.50), 0.999999)
  map2_dbl(x, n, function(k, total) {
    if (is.na(k) || is.na(total) || total <= 0) return(NA_real_)
    binom.test(k, total, conf.level = conf_level)$conf.int[idx]
  })
}

jaccard_set <- function(a, b) {
  u <- union(a, b)
  if (length(u) == 0) return(1.0)
  length(intersect(a, b)) / length(u)
}

# ==============================================================================
# 2. STATEWIDE SPACE-TIME NEAR-DUPLICATE DETECTOR
#    Checks 5 nested tiers across full timestamps (exact Haversine distance):
#      1. Exact Repeat     : 0 min apart, <= 1 m
#      2. M4 Threshold     : <= 2 min apart, <= 100 m
#      3. Tight API Echo   : <= 5 min apart, <= 100 m
#      4. Moderate Window  : <= 15 min apart, <= 100 m
#      5. Same-Street Echo : <= 60 min apart, <= 100 m, same Street or exact GPS
# ==============================================================================
haversine_m <- function(lat1, lng1, lat2, lng2) {
  rad  <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlng <- (lng2 - lng1) * rad
  a    <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlng / 2)^2
  6371000 * 2 * asin(min(1, sqrt(a)))
}

flag_near_duplicates <- function(df, dist_thresh_m = 100) {
  df_ord <- df %>%
    arrange(Start_Time, End_Time, ID) %>%
    mutate(.row_id = row_number())
  
  n <- nrow(df_ord)
  if (n <= 1L) {
    return(df_ord %>% mutate(
      dup_exact_0min        = FALSE,
      dup_2min_100m         = FALSE,
      dup_5min_100m         = FALSE,
      dup_15min_100m        = FALSE,
      dup_60min_same_street = FALSE,
      matched_prev_row      = NA_integer_,
      matched_dt_mins       = NA_real_,
      matched_dist_m        = NA_real_,
      matched_same_severe   = NA
    ))
  }
  
  t_sec  <- as.numeric(df_ord$Start_Time)
  lat    <- as.numeric(df_ord$Start_Lat)
  lng    <- as.numeric(df_ord$Start_Lng)
  st_low <- str_to_lower(coalesce(df_ord$Street, ""))
  sev    <- as.logical(df_ord$severe)
  
  lat_box <- (dist_thresh_m / 111000) * 1.35
  lng_box <- (dist_thresh_m / 84000) * 1.35
  
  dup_0   <- logical(n)
  dup_2   <- logical(n)
  dup_5   <- logical(n)
  dup_15  <- logical(n)
  dup_60  <- logical(n)
  prev_id <- rep(NA_integer_, n)
  prev_dt <- rep(NA_real_, n)
  prev_dm <- rep(NA_real_, n)
  prev_sv <- rep(NA, n)
  
  max_sec <- 3600
  
  for (i in 2:n) {
    if (is.na(lat[i]) || is.na(lng[i])) next
    t_i   <- t_sec[i]
    lat_i <- lat[i]
    lng_i <- lng[i]
    st_i  <- st_low[i]
    
    j <- i - 1L
    while (j >= 1L) {
      dt <- t_i - t_sec[j]
      if (dt > max_sec) break
      
      if (!is.na(lat[j]) && !is.na(lng[j]) &&
          abs(lat_i - lat[j]) <= lat_box &&
          abs(lng_i - lng[j]) <= lng_box) {
        
        d_m <- haversine_m(lat_i, lng_i, lat[j], lng[j])
        
        if (d_m <= dist_thresh_m) {
          exact_gps <- (abs(lat_i - lat[j]) < 1e-5 && abs(lng_i - lng[j]) < 1e-5)
          same_st   <- nzchar(st_i) && nzchar(st_low[j]) && (st_i == st_low[j])
          
          hit_any <- FALSE
          if (dt == 0 && d_m <= 1)  { dup_0[i]  <- TRUE; hit_any <- TRUE }
          if (dt <= 120)            { dup_2[i]  <- TRUE; hit_any <- TRUE }
          if (dt <= 300)            { dup_5[i]  <- TRUE; hit_any <- TRUE }
          if (dt <= 900)            { dup_15[i] <- TRUE; hit_any <- TRUE }
          if (same_st || exact_gps) { dup_60[i] <- TRUE; hit_any <- TRUE }
          
          if (hit_any && is.na(prev_id[i])) {
            prev_id[i] <- j
            prev_dt[i] <- dt / 60
            prev_dm[i] <- d_m
            prev_sv[i] <- (sev[i] == sev[j])
          }
          
          if (dup_0[i] && dup_2[i] && dup_5[i] && dup_15[i] && dup_60[i]) break
        }
      }
      j <- j - 1L
    }
  }
  
  df_ord %>%
    mutate(
      dup_exact_0min        = dup_0,
      dup_2min_100m         = dup_2,
      dup_5min_100m         = dup_5,
      dup_15min_100m        = dup_15,
      dup_60min_same_street = dup_60,
      matched_prev_row      = prev_id,
      matched_dt_mins       = round(prev_dt, 2),
      matched_dist_m        = round(prev_dm, 1),
      matched_same_severe   = prev_sv
    )
}

# ==============================================================================
# 3. QUICK SELF-CHECK ON SYNTHETIC CONTROLS
# ==============================================================================
sim_df <- tibble(
  ID          = c("T1_Index", "T2_Exact0m", "T3_90s_15m", "T4_10m_20m", "T5_1m_500m_Far", "T6_42m_SameGPS"),
  census_name = rep("Fairfax County", 6),
  Start_Time  = ymd_hms(c(
    "2022-01-01 08:00:00",
    "2022-01-01 08:00:00",
    "2022-01-01 08:01:30",
    "2022-01-01 08:10:00",
    "2022-01-01 08:01:00",
    "2022-01-01 08:42:00"
  ), tz = "UTC"),
  End_Time    = ymd_hms(rep("2022-01-01 09:00:00", 6), tz = "UTC"),
  Start_Lat   = c(38.85000, 38.85000, 38.85012, 38.85015, 38.85500, 38.85000),
  Start_Lng   = c(-77.30000, -77.30000, -77.30005, -77.30008, -77.30000, -77.30000),
  Street      = rep("I-66 E", 6),
  severe      = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
)

sim_res <- flag_near_duplicates(sim_df, dist_thresh_m = 100)
get_row <- function(id_str) sim_res %>% filter(ID == id_str)

stopifnot(
  !get_row("T1_Index")$dup_60min_same_street,
  get_row("T2_Exact0m")$dup_exact_0min,
  get_row("T3_90s_15m")$dup_2min_100m,
  !get_row("T4_10m_20m")$dup_5min_100m,
  get_row("T4_10m_20m")$dup_15min_100m,
  !get_row("T5_1m_500m_Far")$dup_60min_same_street,
  !get_row("T6_42m_SameGPS")$dup_15min_100m,
  get_row("T6_42m_SameGPS")$dup_60min_same_street
)
cat("Self-check passed: all 6 synthetic space-time controls verified.\n")

# ==============================================================================
# 4. RUN STATEWIDE SCAN ON FULL VIRGINIA DATASET
# ==============================================================================
cat("Scanning", comma(nrow(acc)), "Virginia records statewide...\n")
t0 <- proc.time()
acc_flagged <- flag_near_duplicates(acc, dist_thresh_m = 100)
cat("Scan completed in", round((proc.time() - t0)[["elapsed"]], 2), "seconds.\n")

tier_specs <- tibble(
  Tier_Col = c(
    "dup_exact_0min",
    "dup_2min_100m",
    "dup_5min_100m",
    "dup_15min_100m",
    "dup_60min_same_street"
  ),
  Tier_Label = c(
    "1. Exact Repeat (0 min, <=1 m)",
    "2. M4 Threshold (<=2 min, <=100 m)",
    "3. Tight API Echo (<=5 min, <=100 m)",
    "4. Moderate Window (<=15 min, <=100 m)",
    "5. Same-Street / Exact-GPS Echo (<=60 min, <=100 m)"
  )
)

# ==============================================================================
# 5. COMPUTE OVERALL SUMMARY, SEVERITY BIAS, RANKING STABILITY & CI IMPACT
# ==============================================================================
n_total   <- nrow(acc_flagged)
n_sev_all <- sum(acc_flagged$severe)
n_non_all <- sum(!acc_flagged$severe)
orig_rate <- n_sev_all / n_total

overall_summary <- pmap_dfr(tier_specs, function(Tier_Col, Tier_Label) {
  flg     <- acc_flagged[[Tier_Col]]
  n_flg   <- sum(flg)
  sev_flg <- sum(flg & acc_flagged$severe)
  non_flg <- sum(flg & !acc_flagged$severe)
  sev_pct <- sev_flg / n_sev_all
  non_pct <- non_flg / n_non_all
  
  p_val <- if ((sev_flg + non_flg) == 0) {
    NA_real_
  } else {
    tryCatch(
      unname(prop.test(c(sev_flg, non_flg), c(n_sev_all, n_non_all))$p.value),
      error = function(e) NA_real_
    )
  }
  
  dedup_rate <- sum(acc_flagged$severe[!flg]) / sum(!flg)
  
  tibble(
    Tier                  = Tier_Label,
    Flagged_Rows          = as.integer(n_flg),
    Overall_Dup_Pct       = round(100 * n_flg / n_total, 2),
    Severe_Dup_Pct        = round(100 * sev_pct, 2),
    NonSevere_Dup_Pct     = round(100 * non_pct, 2),
    Bias_Gap_Pts          = round(100 * (sev_pct - non_pct), 2),
    Bias_p                = round(p_val, 5),
    VA_Rate_Orig_Pct      = round(100 * orig_rate, 2),
    VA_Rate_Dedup_Pct     = round(100 * dedup_rate, 2),
    VA_Rate_Shift_Pts     = round(100 * (dedup_rate - orig_rate), 2)
  )
})

overall_by_year <- acc_flagged %>%
  group_by(Year) %>%
  summarise(
    Total_Rows    = n(),
    Exact_0m_Pct  = round(100 * mean(dup_exact_0min), 2),
    M4_2min_Pct   = round(100 * mean(dup_2min_100m), 2),
    Tight_5m_Pct  = round(100 * mean(dup_5min_100m), 2),
    Mod_15m_Pct   = round(100 * mean(dup_15min_100m), 2),
    Echo_60m_Pct  = round(100 * mean(dup_60min_same_street), 2),
    .groups = "drop"
  )

# Top-5 locality ranking stability (Original vs. Deduplicated)
summarize_localities <- function(df_in) {
  agg <- df_in %>%
    group_by(census_name) %>%
    summarise(
      total_accidents  = n(),
      severe_accidents = sum(severe, na.rm = TRUE),
      .groups = "drop"
    )
  county_pop_base %>%
    left_join(agg, by = "census_name") %>%
    mutate(
      total_accidents  = replace_na(total_accidents, 0L),
      severe_accidents = replace_na(severe_accidents, 0L),
      severe_rate      = ifelse(total_accidents > 0, severe_accidents / total_accidents, 0),
      severe_ratio     = (severe_accidents / population) * 100000
    )
}

loc_orig <- summarize_localities(acc_flagged)

ranking_stability <- pmap_dfr(tier_specs, function(Tier_Col, Tier_Label) {
  loc_dedup <- summarize_localities(acc_flagged %>% filter(!.data[[Tier_Col]]))
  
  t5_cnt_o <- loc_orig  %>% arrange(desc(severe_accidents)) %>% slice_head(n = 5) %>% pull(census_name)
  t5_cnt_d <- loc_dedup %>% arrange(desc(severe_accidents)) %>% slice_head(n = 5) %>% pull(census_name)
  t5_rat_o <- loc_orig  %>% arrange(desc(severe_ratio))     %>% slice_head(n = 5) %>% pull(census_name)
  t5_rat_d <- loc_dedup %>% arrange(desc(severe_ratio))     %>% slice_head(n = 5) %>% pull(census_name)
  t5_pct_o <- loc_orig  %>% filter(total_accidents >= 50) %>% arrange(desc(severe_rate)) %>% slice_head(n = 5) %>% pull(census_name)
  t5_pct_d <- loc_dedup %>% filter(total_accidents >= 50) %>% arrange(desc(severe_rate)) %>% slice_head(n = 5) %>% pull(census_name)
  
  tibble(
    Tier                = Tier_Label,
    Top5_Count_Jaccard  = round(jaccard_set(t5_cnt_o, t5_cnt_d), 3),
    Top5_Ratio_Jaccard  = round(jaccard_set(t5_rat_o, t5_rat_d), 3),
    Top5_Rate_Jaccard   = round(jaccard_set(t5_pct_o, t5_pct_d), 3),
    Top5_Ratio_SameOrd  = identical(t5_rat_o, t5_rat_d)
  )
})

# Page 1 Top-10 Raw Weather Binomial CI Sensitivity (Original vs. <=15 min dedup)
p1_w_orig <- acc_flagged %>%
  filter(!is.na(Weather_Condition), Weather_Condition != "") %>%
  group_by(Weather_Condition) %>%
  summarise(
    Orig_Total  = n(),
    Orig_Severe = sum(severe, na.rm = TRUE),
    Orig_Rate   = Orig_Severe / Orig_Total,
    .groups = "drop"
  ) %>%
  filter(Orig_Total >= 20) %>%
  slice_max(Orig_Rate, n = 10)

cl_w_orig <- 1 - get_adjusted_alpha(max(1, nrow(p1_w_orig)), "bonferroni", 0.05)
p1_w_orig <- p1_w_orig %>%
  mutate(
    Orig_CI_W = calc_binom_ci(Orig_Severe, Orig_Total, "upper", cl_w_orig) -
      calc_binom_ci(Orig_Severe, Orig_Total, "lower", cl_w_orig)
  )

p1_w_dedup <- acc_flagged %>%
  filter(!dup_15min_100m, Weather_Condition %in% p1_w_orig$Weather_Condition) %>%
  group_by(Weather_Condition) %>%
  summarise(
    Dedup15m_Total  = n(),
    Dedup15m_Severe = sum(severe, na.rm = TRUE),
    Dedup15m_Rate   = Dedup15m_Severe / Dedup15m_Total,
    .groups = "drop"
  )

cl_w_dedup <- 1 - get_adjusted_alpha(max(1, nrow(p1_w_dedup)), "bonferroni", 0.05)
p1_w_dedup <- p1_w_dedup %>%
  mutate(
    Dedup15m_CI_W = calc_binom_ci(Dedup15m_Severe, Dedup15m_Total, "upper", cl_w_dedup) -
      calc_binom_ci(Dedup15m_Severe, Dedup15m_Total, "lower", cl_w_dedup)
  )

p1_weather_impact <- p1_w_orig %>%
  left_join(p1_w_dedup, by = "Weather_Condition") %>%
  transmute(
    Weather_Condition,
    Orig_Total,
    Dedup15m_Total,
    Dup_15m_Pct        = round(100 * (Orig_Total - Dedup15m_Total) / pmax(1, Orig_Total), 2),
    Orig_Rate_Pct      = round(100 * Orig_Rate, 2),
    Dedup15m_Rate_Pct  = round(100 * Dedup15m_Rate, 2),
    Rate_Shift_Pts     = round(100 * (Dedup15m_Rate - Orig_Rate), 2),
    CI_Width_Chg_Pct   = round(100 * (Dedup15m_CI_W - Orig_CI_W) / pmax(1e-9, Orig_CI_W), 2)
  )

# ==============================================================================
# 6. PRINT ALL RESULTS DIRECTLY TO RSTUDIO CONSOLE
# ==============================================================================
cat("\n==============================================================================\n")
cat("1. OVERALL VIRGINIA DATASET NEAR-DUPLICATE SHARE (N =", comma(n_total), "ROWS)\n")
cat("==============================================================================\n")
for (r in seq_len(nrow(overall_summary))) {
  cat(sprintf(
    "  %-52s : %6.2f%%  (%s of %s rows)\n",
    overall_summary$Tier[r],
    overall_summary$Overall_Dup_Pct[r],
    comma(overall_summary$Flagged_Rows[r]),
    comma(n_total)
  ))
}

cat("\n==============================================================================\n")
cat("2. OVERALL NEAR-DUPLICATE SHARE (%) BY YEAR\n")
cat("==============================================================================\n")
print(overall_by_year, n = 20, width = Inf)

if ("Source" %in% names(acc_flagged)) {
  cat("\n==============================================================================\n")
  cat("3. OVERALL NEAR-DUPLICATE SHARE (%) BY API SOURCE\n")
  cat("==============================================================================\n")
  acc_flagged %>%
    group_by(Source) %>%
    summarise(
      Total_Rows   = n(),
      Exact_0m_Pct = round(100 * mean(dup_exact_0min), 2),
      M4_2min_Pct  = round(100 * mean(dup_2min_100m), 2),
      Tight_5m_Pct = round(100 * mean(dup_5min_100m), 2),
      Mod_15m_Pct  = round(100 * mean(dup_15min_100m), 2),
      Echo_60m_Pct = round(100 * mean(dup_60min_same_street), 2),
      .groups = "drop"
    ) %>%
    print(n = 20, width = Inf)
}

cat("\n==============================================================================\n")
cat("4. OVERALL SEVERITY BIAS (SEVERE 3-4 vs. NON-SEVERE 1-2)\n")
cat("==============================================================================\n")
print(
  overall_summary %>%
    select(Tier, Overall_Dup_Pct, Severe_Dup_Pct, NonSevere_Dup_Pct,
           Bias_Gap_Pts, Bias_p, VA_Rate_Orig_Pct, VA_Rate_Dedup_Pct, VA_Rate_Shift_Pts),
  n = 10, width = Inf
)

cat("\n==============================================================================\n")
cat("5. TOP 5 LOCALITY RANKING STABILITY (1.000 = IDENTICAL TOP 5 SET)\n")
cat("==============================================================================\n")
print(ranking_stability, n = 10, width = Inf)

cat("\n==============================================================================\n")
cat("6. PAGE 1 TOP 10 WEATHER BINOMIAL CI IMPACT (ORIGINAL vs. <=15 MIN DEDUP)\n")
cat("==============================================================================\n")
print(p1_weather_impact, n = 15, width = Inf)

cat("\n==============================================================================\n")
cat("7. SAMPLE OF 10 FLAGGED NEAR-DUPLICATE PAIRS (<=15 MIN, <=100 M)\n")
cat("==============================================================================\n")
sample_idx <- head(which(acc_flagged$dup_15min_100m), 10)
if (length(sample_idx) > 0) {
  cur  <- acc_flagged[sample_idx, ]
  prev <- acc_flagged[cur$matched_prev_row, ]
  tibble(
    County     = cur$census_name,
    Gap_Mins   = cur$matched_dt_mins,
    Dist_m     = cur$matched_dist_m,
    Prev_Time  = format(prev$Start_Time, "%Y-%m-%d %H:%M:%S"),
    Curr_Time  = format(cur$Start_Time, "%Y-%m-%d %H:%M:%S"),
    Prev_Sev   = prev$Severity,
    Curr_Sev   = cur$Severity,
    Street     = cur$Street
  ) %>%
    print(n = 10, width = Inf)
} else {
  cat("No pairs flagged under <=15 min and <=100 m.\n")
}

# ==============================================================================
# 7. OVERALL SUMMARY CHART (RSTUDIO PLOTS PANE ONLY — NOT SAVED TO DISK)
# ==============================================================================
p_overall <- overall_summary %>%
  mutate(Tier = factor(Tier, levels = rev(tier_specs$Tier_Label))) %>%
  ggplot(aes(x = Tier, y = Overall_Dup_Pct)) +
  geom_col(fill = "#0F2042", width = 0.62, alpha = 0.92) +
  geom_text(
    aes(label = paste0(sprintf("%.2f%%", Overall_Dup_Pct), " (", comma(Flagged_Rows), " rows)")),
    hjust = -0.08, size = 3.5, fontface = "bold", color = "#1A202C"
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.30))) +
  labs(
    title    = paste0("Overall Virginia Dataset Near-Duplicate Share (N = ", comma(n_total), " Rows)"),
    subtitle = "Statewide space-time check across full 2016–2023 timestamps (Haversine distance <= 100 m)",
    x        = NULL,
    y        = "Share of All Virginia Records Flagged (%)"
  ) +
  theme_minimal(base_size = 11.5) +
  theme(axis.text.y = element_text(face = "bold"))

print(p_overall)

# 1. Independent cross-check (does not rely on flag_near_duplicates loop)
tibble(
  Definition = c(
    "Loop: 0 min & <=1 m",
    "Exact same Start_Time + Start_Lat + Start_Lng",
    "Same Start_Time + GPS + End_Time",
    "Same Start_Time + GPS + Severity + Description",
    "Full row clone (everything identical except ID)"
  ),
  Duplicate_Rows = c(
    sum(acc_flagged$dup_exact_0min),
    sum(duplicated(acc[c("Start_Time", "Start_Lat", "Start_Lng")])),
    sum(duplicated(acc[c("Start_Time", "End_Time", "Start_Lat", "Start_Lng")])),
    sum(duplicated(acc[c("Start_Time", "Start_Lat", "Start_Lng", "Severity", "Description")])),
    sum(duplicated(acc %>% select(-ID)))
  )
) %>%
  mutate(Pct_of_303301 = round(100 * Duplicate_Rows / nrow(acc), 2)) %>%
  print(width = Inf)

# 2. See 10 actual repeated groups (20 rows) side-by-side
acc %>%
  group_by(Start_Time, Start_Lat, Start_Lng) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(Start_Time, Start_Lat, Start_Lng) %>%
  select(any_of(c("ID", "Source", "census_name", "Start_Time", "End_Time",
                  "Severity", "Street", "Description"))) %>%
  slice_head(n = 20) %>%
  print(n = 20, width = Inf)