# -------------------------------------
# Script: 01_washout_misuse.R
# Author: Nick Williams
# Purpose:
# Notes:
# -------------------------------------

library(tidyverse)
library(lubridate)
library(fst)
library(collapse)

source("R/helpers.R")

# Load cohort
for (i in c("", "_7_day_gap"))
{
  cohort <- load_data(paste0("msk_washout_continuous_enrollment_opioid_requirements_with_exposures", i, ".fst"))
# Load study pain opioids
opioids <- load_data(paste0("msk_washout_continuous_enrollment_opioid_requirements_pain_opioids_dts", i, ".fst"))

washout_oud_misuse <- 
  fsubset(opioids, RX_FILL_DT %within% interval(washout_start_dt, washout_end_dt)) |> 
  fgroup_by(BENE_ID) |> 
  fsummarise(distinct_providers = n_distinct(PRSCRBNG_PRVDR_NPI), 
             distinct_dispensers = n_distinct(DSPNSNG_PRVDR_NPI),
             total_days_supply = sum(DAYS_SUPPLY)) |> 
  fmutate(
    score_providers = case_when(
      distinct_providers <= 2 ~ 0,
      distinct_providers <= 4 ~ 1,
      distinct_providers >= 5 ~ 2
    ),
    score_dispensers = case_when(
      distinct_dispensers <= 2 ~ 0,
      distinct_dispensers <= 4 ~ 1,
      distinct_dispensers >= 5 ~ 2
    ),
    score_days_supply = case_when(
      total_days_supply <= 185 ~ 0,
      total_days_supply <= 240 ~ 1,
      total_days_supply > 240 ~ 2,
      is.na(total_days_supply) ~ 0
    ), 
    exclusion_oud_misuse = as.numeric((score_providers +  score_dispensers + score_days_supply) >= 5)
  ) |> 
  fselect(BENE_ID, exclusion_oud_misuse) |> 
  filter(exclusion_oud_misuse == 1) |>
  join(cohort, how = "right") |> 
  fmutate(exclusion_oud_misuse = replace_na(exclusion_oud_misuse, 0))

# export
write_data(washout_oud_misuse, paste0("msk_washout_continuous_enrollment_opioid_requirements_washout_oud_misuse", i, ".fst"))
}
