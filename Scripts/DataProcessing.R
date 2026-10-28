# Install any missing packages once if needed:
# install.packages(c("tidyverse", "data.table", "sf", "tigris"))

library(tidyverse)  # Loads dplyr, tidyr, readr, stringr, ggplot2
library(data.table) # For fast reading of the large US_Accidents CSV
library(readxl)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(tidyverse)

# ==========================================================================
# STEP 1: FAST FILTER TO VIRGINIA & SAVE SUBSET
# ==========================================================================

#Bring Full table in
dffull<- fread("../Data/US_Accidents_March23.csv")%>%
  as_tibble()

# Filter full table as just state
dfva <- dffull |>
  filter(State == "VA")

### ---- Verify that the data brought in has the same dimensions as the full dataset
# Verify dfva is the same column dimensions as the full
check_col <- function(x, y){
  stopifnot(ncol(x) == ncol(y))   
  invisible(TRUE)
}

#create corruption (AI)
set.seed(123)
dfvacor<-dfva |> 
  mutate(corcol = rnorm(nrow(dfva), mean = 50, sd = 10))

#Verify works on original and fails on corrupted.
c(passes_originalnew = !inherits(try(check_col(dffull, dfva), silent = TRUE), "try-error"),
  rejects_corruptnew =  inherits(try(check_col(dffull, dfvacor), silent = TRUE), "try-error"))

#---------------------
### --- Verify ALL Virginia observations were pulled in (Completeness & Alias Check) AI generated
check_va_complete <- function(x, y) {
  # 1. Count exact "VA" matches in the full dataset
  expected_n <- sum(x$State == "VA", na.rm = TRUE)
  # 2. Standardize strings (uppercase, trim whitespace, remove periods)
  clean_state <- gsub("\\.", "", trimws(toupper(x$State)))
  # 3. Count all possible Virginia representations ("VA", "VIRGINIA", "VA.", etc.)
  clean_n <- sum(clean_state %in% c("VA", "VIRGINIA"), na.rm = TRUE)
  # Verify:
  # - All rows in sub_df are strictly "VA"
  # - Number of rows in sub_df equals the exact "VA" count in full_df
  # - No rows in full_df were spelled "Virginia", "va", or "Va." and left behind
  stopifnot(
    !anyNA(y$State),
    all(y$State == "VA"),
    nrow(y) == expected_n,
    expected_n == clean_n
  )
  invisible(TRUE)
}

# Create corruption 1: Simulate dropping 10 rows from dfva
dfva_incomplete <- dfva |> 
  slice(-c(1:10))

# Create corruption 2: Simulate someone typing "Virginia" instead of "VA" in dffull
dffull_bad_spelling <- dffull
dffull_bad_spelling$State[which(dffull$State == "VA")[1]] <- "Virginia"

# Verify works on original and fails on both corruptions
c(passes_original        = !inherits(try(check_va_complete(dffull, dfva), silent = TRUE), "try-error"),
  rejects_dropped_rows   =  inherits(try(check_va_complete(dffull, dfva_incomplete), silent = TRUE), "try-error"),
  rejects_virginia_spell =  inherits(try(check_va_complete(dffull_bad_spelling, y), silent = TRUE), "try-error"))
# Write VA Only Dataset------------------------------

write_csv(dfva, "../Data/Virginia_Accidents.csv")
gc()
#Remove Unneeded memory from R Global Environment.----------------------
rm(dfvacor,dffull, dffull_bad_spelling, dfva_incomplete)



# ==========================================================================
# FETCH & CLEAN OFFICIAL US CENSUS POPULATION DATA FOR VIRGINIA
# ==========================================================================
census_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv"

va_pop <- read_csv(
  census_url,
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
) %>%
  filter(STNAME == "Virginia", SUMLEV == "050") %>% # 050 = All 133 VA Counties & Independent Cities
  transmute(
    GEOID       = paste0(STATE, COUNTY),
    census_name = str_squish(CTYNAME),
    census_key  = str_to_lower(census_name),
    population  = POPESTIMATE2023
  )

# ==========================================================================
#  HARMONIZE VIRGINIA COUNTY & INDEPENDENT CITY NAMES
# ==========================================================================
# The 34 VA independent cities that do NOT share a name with a surrounding county
unambiguous_va_cities <- c(
  "alexandria", "bristol", "buena vista", "charlottesville", "chesapeake",
  "colonial heights", "covington", "danville", "emporia", "falls church",
  "fredericksburg", "galax", "hampton", "harrisonburg", "hopewell",
  "lexington", "lynchburg", "manassas", "manassas park", "martinsville",
  "newport news", "norfolk", "norton", "petersburg", "poquoson",
  "portsmouth", "radford", "salem", "staunton", "suffolk",
  "virginia beach", "waynesboro", "williamsburg", "winchester"
)

dfva_clean <- dfva %>%
  mutate(
    # 1. Standardize encoding, remove parentheses around "(city)" or "(county)", and lowercase
    cnty_raw   = str_to_lower(str_squish(iconv(County, to = "UTF-8", sub = ""))),
    cnty_clean = str_squish(str_replace_all(cnty_raw, "[()]", "")),
    
    # 2. Extract base name by stripping trailing " county" or " city"
    cnty_base  = str_remove(cnty_clean, "\\s+(county|city)$"),
    
    # 3. Map every jurisdiction 1-to-1 from the County column to the Census CTYNAME format
    census_key = case_when(
      # Fix the 2 Virginia counties that literally have "City" in their county name
      cnty_base %in% c("james", "james city") ~ "james city county",
      cnty_base %in% c("charles", "charles city") ~ "charles city county",
      
      # Bedford city merged into Bedford County in 2013
      cnty_base == "bedford" ~ "bedford county",
      
      # Resolve the 4 VA County vs. Independent City collisions (Fairfax, Roanoke, Richmond, Franklin)
      # Because raw independent cities are labeled "(city)" or "city", keep them as "city", 
      # and map plain "Fairfax" / "Roanoke" / "Richmond" / "Franklin" directly to "county"
      cnty_clean %in% c("fairfax city", "roanoke city", "richmond city", "franklin city") ~ cnty_clean,
      str_detect(cnty_clean, "^city of (fairfax|roanoke|richmond|franklin)$") ~ paste0(str_remove(cnty_clean, "^city of "), " city"),
      cnty_base %in% c("fairfax", "roanoke", "richmond", "franklin") ~ paste0(cnty_base, " county"),
      
      # Append " city" to the 34 unambiguous independent cities
      cnty_base %in% unambiguous_va_cities ~ paste0(cnty_base, " city"),
      
      # Append " county" to all remaining Virginia counties
      !is.na(cnty_base) & cnty_base != "" ~ paste0(cnty_base, " county"),
      TRUE ~ NA_character_
    )
  )



#---------------------------------

# Verify 100% match rate against Census
unmatched <- dfva_clean %>%
  anti_join(va_pop, by = "census_key") %>%
  count(County, City, census_key, sort = TRUE)

print(unmatched)

# 2. Left join population data onto the accident records and remove helper columns
dfva_joined <- dfva_clean %>%
  left_join(va_pop, by = "census_key") %>%
  select(-cnty_raw, -cnty_clean, -cnty_base)

View(dfva_joined)

# ==========================================================================
# STEP 5 SPATIAL AUDIT COMPARING COUNTY TEXT VS. GPS COORDINATES
# ==========================================================================


# cb = FALSE uses full legal boundaries (including bridges over water in Hampton Roads/Arlington)
va_polygons <- counties(state = "VA", cb = FALSE, year = 2022) %>%
  transmute(
    spatial_GEOID = GEOID,
    spatial_name  = NAMELSAD,
    spatial_key   = str_to_lower(str_squish(NAMELSAD))
  )

dfva_audit <- dfva_clean %>%
  filter(!is.na(Start_Lng), !is.na(Start_Lat)) %>%
  st_as_sf(coords = c("Start_Lng", "Start_Lat"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(va_polygons)) %>%
  st_join(va_polygons, join = st_within) %>%
  st_drop_geometry()

audit_summary <- dfva_audit %>%
  summarise(
    total_rows       = n(),
    exact_matches    = sum(census_key == spatial_key, na.rm = TRUE),
    mismatches       = sum(census_key != spatial_key, na.rm = TRUE),
    outside_va_poly  = sum(is.na(spatial_key)),
    match_percentage = round((exact_matches / total_rows) * 100, 2)
  )

print(audit_summary)

mismatch_breakdown <- dfva_audit %>%
  filter(census_key != spatial_key | is.na(spatial_key)) %>%
  count(County, City, text_assigned = census_key, gps_assigned = spatial_key, sort = TRUE)

View(mismatch_breakdown)

# Summary Table Formulations -----
dfva_county_summary <- dfva_joined %>%
  group_by(GEOID, census_name, population) %>%
  summarise(
    total_accidents  = n(),
    severe_accidents = sum(Severity >= 3, na.rm = TRUE),
    avg_severity     = mean(Severity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    accidents_per_person = total_accidents / population,
    accidents_per_100k   = (total_accidents / population) * 100000,
    severe_per_100k      = (severe_accidents / population) * 100000
  ) %>%
  arrange(desc(accidents_per_100k))

# Inspect final outputs
View(dfva_county_summary)
head(dfva_county_summary, 20)
View(dfva_joined)
##Remove Unneeded Memory from R image-------------------------------------
rm(dfva, dfva_clean, dfva_audit)

# Save full row-level Virginia dataset with Census population attached
write_csv(dfva_joined, "../Data/Virginia_Accidents_With_Pop.csv")