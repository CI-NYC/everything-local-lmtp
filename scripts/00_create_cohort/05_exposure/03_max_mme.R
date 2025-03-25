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

source("R/helpers.R")

# load cohort and opioid data
cohort <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst")
opioids <- load_data("exposure_period_opioids.fst")

setDT(opioids)
setkey(opioids, BENE_ID)

opioids <- opioids[, .(BENE_ID, msk_diagnosis_dt, exposure_end_dt, 
                       rx_start_dt, rx_end_dt, NDC, opioid, mme_strength_per_day)] |>
  mutate(rx_end_dt = rx_end_dt - days(1)) # true exposure end date should be 1 less than what was previously used

num_opioids <- opioids |>
  group_by(BENE_ID) |>
  summarize(num_opioids = n())

saveRDS(num_opioids, "/mnt/general-data/disability/everything-local-lmtp/num_opioids.rds")

# Calculate max daily dose -----------------------------------------------------

opioids <- opioids[, list(data = list(data.table(.SD))), by = BENE_ID]

calculate_max_daily_dose <- function(data) {
  to_modify <- copy(data)
  
  to_modify[, .(date = seq(rx_start_dt, rx_end_dt, by = "1 day"), 
                exposure_end_dt, NDC, opioid, mme_strength_per_day), 
            by = .(seq_len(nrow(data)))
            ][date <= exposure_end_dt, 
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

write_data(out, "exposure_max_daily_dose_mme.fst")
