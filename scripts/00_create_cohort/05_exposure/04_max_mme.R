# -------------------------------------
# Script: 03_max_mme.R
# Author: Nick Williams
# Purpose: Calculate max daily dose (MME) for opioids prescribed during the exposure period
# Notes: Modified from https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/01_create_treatments/02_06mo/10_mme/02_treatment_max_daily_dose_mme.R
# -------------------------------------

library(tidyverse)
library(readxl)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)
library(collapse)

source("R/helpers.R")

# load cohort and opioid data
cohort <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst")

days_supply <- load_data("exposure_days_supply.fst") # get exposure end dates

cohort <- cohort |>
  left_join(days_supply) |>
  select(BENE_ID, last_exposure_opioid_dt)

opioids <- load_data("exposure_period_opioids.fst") |>
  left_join(cohort)|>
  filter(rx_start_dt <= last_exposure_opioid_dt) |> # only keeping start date before episode end period
  mutate(rx_end_dt = ifelse(last_exposure_opioid_dt < rx_end_dt, last_exposure_opioid_dt, rx_end_dt)) |> # if rx_end_dt is after episode end date, then use last_exposure_opioid_dt as final date
  mutate(rx_end_dt = as.Date(rx_end_dt))

# rewrite over exposure period data
write_data(opioids, "exposure_period_opioids_ACTUAL.fst")

setDT(opioids)
setkey(opioids, BENE_ID)

opioids <- opioids[, .(BENE_ID, min_opioid_date, last_exposure_opioid_dt, 
                       rx_start_dt, rx_end_dt, NDC, opioid, mme_strength_per_day)]

num_opioids <- opioids |>
  group_by(BENE_ID) |>
  summarize(num_opioids = n())

saveRDS(num_opioids, "/mnt/general-data/disability/everything-local-lmtp/num_opioids.rds")

# Calculate max daily dose -----------------------------------------------------

opioids <- opioids[, list(data = list(data.table(.SD))), by = BENE_ID]

calculate_max_daily_dose <- function(data) {
  #data <- (opioids |> filter(BENE_ID == "HHHHHH44eACnAnH"))$data |> as.data.table() 
  to_modify <- copy(data) |>
    as.data.table()
  
  to_modify[, .(date = seq(rx_start_dt, rx_end_dt, by = "1 day"), 
                last_exposure_opioid_dt, NDC, opioid, mme_strength_per_day), 
            by = .(seq_len(nrow(data)))
            ][date <= last_exposure_opioid_dt, 
              ][, .(total_mme_strength = sum(mme_strength_per_day, na.rm = TRUE)), 
                by = .(date)
                ][, .(exposure_max_daily_dose_mme = max(total_mme_strength))]
}

# TEST function need to test manually
# testthat::test_that(
#   "Test calculate_max_daily_dose function works as expected",
#   testthat::expect_equal({
#     fsubset(opioids, BENE_ID %==% "HHHHHH4477eBBkH")$data |>
#       calculate_max_daily_dose()
#   }, 30)
# )

plan(multisession, workers = 50)

# Apply function
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                 out <- calculate_max_daily_dose(data)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)

testthat::test_that(
  "All observations have a max daily MME",
  testthat::expect_false(any(is.na(out$exposure_max_daily_dose_mme)))
)

write_data(out, "exposure_max_daily_dose_mme.fst")

######## 7 day gap

rm(cohort)
rm(out)
rm(days_supply)

cohort <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst")


days_supply <- load_data("exposure_days_supply_7_day_gap.fst") # get episode end dates

cohort <- cohort |>
  left_join(days_supply) |>
  select(BENE_ID, last_exposure_opioid_dt)

opioids <- load_data("exposure_period_opioids.fst") |>
  left_join(cohort)|>
  filter(rx_start_dt <= last_exposure_opioid_dt) |> # only keeping start date before eouside end period
  mutate(rx_end_dt = ifelse(last_exposure_opioid_dt < rx_end_dt, last_exposure_opioid_dt, rx_end_dt)) |> # if rx_end_dt is after episode end date, then use last_exposure_opioid_dt as final date
  mutate(rx_end_dt = as.Date(rx_end_dt))

# rewrite over exposure period data
write_data(opioids, "exposure_period_opioids_7_day_gap_ACTUAL.fst")

setDT(opioids)
setkey(opioids, BENE_ID)

opioids <- opioids[, .(BENE_ID, min_opioid_date, last_exposure_opioid_dt, 
                       rx_start_dt, rx_end_dt, NDC, opioid, mme_strength_per_day)]

num_opioids <- opioids |>
  group_by(BENE_ID) |>
  summarize(num_opioids = n())

saveRDS(num_opioids, "/mnt/general-data/disability/everything-local-lmtp/num_opioids_7_day_gap.rds")

# Calculate max daily dose -----------------------------------------------------

opioids <- opioids[, list(data = list(data.table(.SD))), by = BENE_ID]

calculate_max_daily_dose <- function(data) {
  #data <- (opioids |> filter(BENE_ID == "HHHHHHddnCennB7"))$data |> as.data.table() 
  to_modify <- copy(data) |>
    as.data.table()
  
  to_modify[, .(date = seq(rx_start_dt, rx_end_dt, by = "1 day"), 
                last_exposure_opioid_dt, NDC, opioid, mme_strength_per_day), 
            by = .(seq_len(nrow(data)))
  ][date <= last_exposure_opioid_dt, 
  ][, .(total_mme_strength = sum(mme_strength_per_day, na.rm = TRUE)), 
    by = .(date)
  ][, .(exposure_max_daily_dose_mme = max(total_mme_strength))]
}

plan(multisession, workers = 50)

# Apply function
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                 out <- calculate_max_daily_dose(data)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)

testthat::test_that(
  "All observations have a max daily MME",
  testthat::expect_false(any(is.na(out$exposure_max_daily_dose_mme)))
)

write_data(out, "exposure_max_daily_dose_mme_7_day_gap.fst")
