# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
library(data.table)

source("~/everything-local-lmtp/R/helpers.R")

# getting all opioids for cohort within exposure period
opioids <- load_data("exposure_period_opioids.fst") |>
  select(BENE_ID, exposure_end_dt, rx_end_dt) |>
  mutate(rx_end_dt = case_when(rx_end_dt > exposure_end_dt ~ exposure_end_dt, # only looking until the end of the exposure period
                               TRUE ~ rx_end_dt - days(1) # because rx_end_dt is one more than should be
                               ))

# finding latest opioid date per beneficiary
opioids_grouped <- opioids |>
  group_by(BENE_ID) |>
  summarize(latest_opioid_date = max(rx_end_dt)) |>
  as.data.table()

# getting ED visits
claims <- readRDS("/mnt/general-data/disability/pain-severity/intermediate/visits_cleaned_with_procedures_and_inpatients_excluded.rds")

claims <- unique(merge(claims, opioids_grouped, by = "BENE_ID"))

# filter within the timeframe (1 month preceding the exposure)
claims_exposure_period <- claims[start_dt %within% interval(latest_opioid_date - days(30),
                                                          latest_opioid_date),
                               .(BENE_ID, ED_visit_dt = start_dt, latest_opioid_date, LINE_PRCDR_CD)]


######## 1 ED VISIT
# Create indicator variable for whether or not a patient had claim in mediator period
# Right join with cohort
claims_exposure_period <- claims_exposure_period[, .(has_1_ED_visit_exposure = as.numeric(.N > 0), 
                                                 has_2plus_ED_visit_exposure = as.numeric(.N > 1)), by = "BENE_ID"]

claims_final <- merge(claims_exposure_period, 
                      opioids_grouped[, .(BENE_ID)], 
                      all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a claim
fix <- c("has_1_ED_visit_exposure", "has_2plus_ED_visit_exposure")
claims_final[, (fix) := lapply(.SD, \(x) fifelse(is.na(x), 0, x)), .SDcols = fix]

saveRDS(claims_final, file.path("/mnt/general-data/disability/everything-local-lmtp", "confounder_num_ED_visit.rds"))