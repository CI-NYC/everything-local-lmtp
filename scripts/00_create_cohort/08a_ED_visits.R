# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
library(data.table)

source("~/everything-local-lmtp/R/helpers.R")

for (i in c("", "_7_day_gap"))
{
  
  # if episode is greater than or equal to 1 month, use the episode period, if episode is less than 1 month, use 1 month preceding the last exposure opioid date
cohort <- load_data(paste0("msk_cohort", i, ".fst")) |>
  mutate(num_days_of_exposure_coverage = as.numeric(last_exposure_opioid_dt - (washout_end_dt + 1) + 1)) |> # number of days in continuous opioid use episode
  mutate(lookback_start = case_when(num_days_of_exposure_coverage >= 31 ~ washout_end_dt + 1, # first day of exposure
                                    TRUE ~ last_exposure_opioid_dt - 30), # results in a 31 day period
         lookback_end = last_exposure_opioid_dt) |>
  mutate(num_days_of_exposure_coverage = ifelse(num_days_of_exposure_coverage < 31, 31, num_days_of_exposure_coverage)) |> # 1 month lookback period for those with less than 1 month opioid episode
  select(BENE_ID, lookback_start, lookback_end, num_days_of_exposure_coverage) |>
  as.data.table()

# getting ED visits
claims <- readRDS("/mnt/general-data/disability/pain-severity/intermediate/visits_cleaned_with_procedures_and_inpatients_excluded.rds")

claims <- unique(merge(claims, cohort, by = "BENE_ID"))

# filter within the timeframe
claims_exposure_period <- claims[start_dt %within% interval(lookback_start,
                                                            lookback_end),
                               .(BENE_ID, lookback_start, ED_visit_dt = start_dt, lookback_end, num_days_of_exposure_coverage, CLM_ID, LINE_PRCDR_CD)]


######## 1 ED VISIT
# Create indicator variable for whether or not a patient had an average of 2+ ED visits per month in the period
# Right join with cohort
# claims_exposure_period <- claims_exposure_period[, .(has_1_ED_visit_exposure = as.numeric(.N > 0), 
#                                                  has_2plus_ED_visit_exposure = as.numeric(.N > 1)), by = "BENE_ID"]

claims_exposure_period <- claims_exposure_period[, .(has_2plus_ED_visit_exposure = as.numeric((.N/num_days_of_exposure_coverage) * 31 >= 2)), by = c("BENE_ID", "num_days_of_exposure_coverage")]

claims_final <- merge(claims_exposure_period, 
                      cohort[, .(BENE_ID)], 
                      all.y = TRUE, by = "BENE_ID") |>
  select(-c(num_days_of_exposure_coverage))

# Convert NAs to 0 for observations in the cohort that didn't have a claim
fix <- c("has_2plus_ED_visit_exposure")
claims_final[, (fix) := lapply(.SD, \(x) fifelse(is.na(x), 0, x)), .SDcols = fix] 

saveRDS(claims_final, file.path("/mnt/general-data/disability/everything-local-lmtp", paste0("confounder_num_ED_visit", i, ".rds")))
}