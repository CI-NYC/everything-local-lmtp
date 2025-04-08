# -------------------------------------
# Script: 05_combine_exposures.R
# Author: Nick Williams
# Purpose: Combine two exposures and define exposure subsets
# Notes:
# -------------------------------------

library(collapse)
library(tidyverse)
library(fst)

source("R/helpers.R")

mme <- load_data("exposure_max_daily_dose_mme.fst")
days_supply <- load_data("exposure_days_supply.fst")

exposures <- 
  join(mme, days_supply, how = "full") |> 
  fmutate(subset_B1 = exposure_max_daily_dose_mme >= 50, 
          subset_B2 = exposure_days_supply > 7, 
          subset_B3 = subset_B1 & subset_B2,
          subset_B4 = ifelse(exposure_max_daily_dose_mme >= 90, TRUE, FALSE),
          subset_B5 = ifelse(exposure_max_daily_dose_mme >= 90 & exposure_days_supply > 7, TRUE, FALSE),
          subset_B6 = ifelse(exposure_days_supply > 30, TRUE, FALSE),
          subset_B7 = ifelse(exposure_days_supply > 30 & exposure_max_daily_dose_mme >= 50, TRUE, FALSE),
          subset_B8 = ifelse(exposure_days_supply > 30 & exposure_max_daily_dose_mme >= 90, TRUE, FALSE),
          subset_B_not_risky_days = ifelse(exposure_days_supply <= 7, TRUE, FALSE),
          subset_B_under_20 = ifelse(exposure_max_daily_dose_mme <= 20, TRUE, FALSE),
          subset_B_days_7_dose_under_20 = ifelse(exposure_days_supply <= 7 & exposure_max_daily_dose_mme <= 20, TRUE, FALSE))

write_data(exposures, "exposures_with_subsets.fst")

cohort <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst")

cohort <- cohort |>
  left_join(exposures)

write_data(cohort, "msk_washout_continuous_enrollment_opioid_requirements_with_exposures.fst")


## 7 day gap
rm(cohort)
rm(mme)
rm(days_supply)

mme <- load_data("exposure_max_daily_dose_mme_7_day_gap.fst")
days_supply <- load_data("exposure_days_supply_7_day_gap.fst")

exposures <- 
  join(mme, days_supply, how = "full") |> 
  fmutate(subset_B1 = exposure_max_daily_dose_mme >= 50, 
          subset_B2 = exposure_days_supply > 7, 
          subset_B3 = subset_B1 & subset_B2,
          subset_B4 = ifelse(exposure_max_daily_dose_mme >= 90, TRUE, FALSE),
          subset_B5 = ifelse(exposure_max_daily_dose_mme >= 90 & exposure_days_supply > 7, TRUE, FALSE),
          subset_B6 = ifelse(exposure_days_supply > 30, TRUE, FALSE),
          subset_B7 = ifelse(exposure_days_supply > 30 & exposure_max_daily_dose_mme >= 50, TRUE, FALSE),
          subset_B8 = ifelse(exposure_days_supply > 30 & exposure_max_daily_dose_mme >= 90, TRUE, FALSE),
          subset_B_not_risky_days = ifelse(exposure_days_supply <= 7, TRUE, FALSE),
          subset_B_under_20 = ifelse(exposure_max_daily_dose_mme <= 20, TRUE, FALSE),
          subset_B_days_7_dose_under_20 = ifelse(exposure_days_supply <= 7 & exposure_max_daily_dose_mme <= 20, TRUE, FALSE))

write_data(exposures, "exposures_with_subsets_7_day_gap.fst")

cohort <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst")

cohort <- cohort |>
  left_join(exposures)

write_data(cohort, "msk_washout_continuous_enrollment_opioid_requirements_with_exposures_7_day_gap.fst")
