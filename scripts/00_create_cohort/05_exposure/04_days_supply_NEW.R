# -------------------------------------
# Script: 04_days_supply.R
# Author: Shodai Inose
# Purpose: Calculate the nonoverlapping total days supply of opioids (i.e., a value bounded between 1 and 91)
# Notes: Modified from https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/01_create_treatments/02_06mo/09_treatment_proportion_days_covered.R
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

opioids <- opioids[, .(BENE_ID, min_opioid_date, exposure_end_dt, 
                       rx_start_dt, rx_end_dt)] |>
  arrange(BENE_ID, rx_start_dt, rx_end_dt) |>
  distinct()

# Calculate duration -----------------------------------------------------

opioids <- opioids[, list(data = list(data.table(.SD))), by = BENE_ID]

get_duration <- function(data, gap = 30) {
  #data <- opioids$data[[158]] |> as.data.table() for testing
  # gap <- 30
  to_modify <- copy(data) |>
    as.data.table()
  
  data <- data |>
    as.data.table()

  # all dates with some opioid prescription
  opioid_dates <- to_modify[, .(date = seq(rx_start_dt, rx_end_dt, by = "1 day"), 
                exposure_end_dt), 
            by = .(seq_len(nrow(to_modify)))
  ][date <= exposure_end_dt, 
  ][, exposure_end_dt := NULL][, seq_len := 1] |> distinct()
  
  # all dates in hypothetical exposure period
  all_dates_exposure_period <- data[, .(date = seq(min_opioid_date, exposure_end_dt, by = "1 day")), 
            by = .(seq_len(nrow(data)))
  ][seq_len == 1][, seq_len := NULL]
  
  # join opioid dates with all possible dates in exposure period
  all_dates_exposure_period <- merge(all_dates_exposure_period, opioid_dates, by = "date", all.x = TRUE)[, seq_len := ifelse(is.na(seq_len), 0, seq_len)]
  
  # get cumulative day sum
  all_dates_exposure_period[, opioid_days := cumsum(seq_len)]
  
  # group by instance to identify gaps (anything > 0 indicates a gap of X days)
  all_dates_exposure_period[, num_days_in_gap := .N - 1, by = opioid_days]
  
  # find FIRST instance of 30+ day gap -- this is the last day of exposure
  all_dates_exposure_period[, indicator_30_plus_day_gap := as.integer(.I == min(.I[num_days_in_gap > gap])), by = opioid_days]
  
  all_dates_exposure_period[indicator_30_plus_day_gap == 1]$opioid_days
  
}

plan(multisession, workers = 1)

# Apply function
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                 out <- get_duration(data, gap = 30)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)

# TEST function
# testthat::test_that(
#   "Test days_supply function works as expected",
#   testthat::expect_equal({
#     fsubset(opioids, BENE_ID %==% "HHHHHH447777ddB") |> 
#       days_supply()
#   }, 76)
# )
# 
# testthat::test_that(
#   "Test days_supply function works as expected",
#   testthat::expect_equal({
#     fsubset(opioids, BENE_ID %==% "HHHHHH447AddkCB") |> 
#       days_supply()
#   }, 25)
# )
# 
# testthat::test_that(
#   "Test days_supply function works as expected",
#   testthat::expect_equal({
#     fsubset(opioids, BENE_ID %==% "HHHHHH44Ak7AnnH") |> 
#       days_supply()
#   }, 28)
# )
# 
# testthat::test_that(
#   "Test days_supply function works as expected",
#   testthat::expect_equal({
#     fsubset(opioids, BENE_ID %==% "HHHHHH4477d4Bnd") |> 
#       days_supply()
#   }, 30)
# )

write_data(out, "exposure_days_supply.fst")

plan(multisession, workers = 1)

# Apply function
out_seven <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                 out <- get_duration(data, gap = 7)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)

write_data(out_seven, "exposure_days_supply_seven.fst")


