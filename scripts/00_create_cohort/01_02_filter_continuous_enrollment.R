# -------------------------------------
# Script: 01_02_filter_continuous_enrollment.R
# Author: Nick Williams
# Purpose: Create continuous enrollment periods and filter 
#   to those periods within the study timeframe.
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(doFuture)
library(dplyr)

source("R/helpers.R")

# Load washout dates
washout <- load_data("msk_washout_dts.fst")

# Load temporary files for 01_01_filter_continuous_enrollment.R
files <- 
  "/mnt/general-data/disability/everything-local-lmtp/tmp" |> 
  list.files(full.names = TRUE)

#' Creates continuous enrollment periods
find_enrollment_periods <- function(data) {
  # If there is only one row, return that row directly
  if (nrow(data) == 1) {
    out <- data.table(BENE_ID = data$BENE_ID, 
                      washout_start_dt = data$washout_start_dt, 
                      msk_diagnosis_dt = data$msk_diagnosis_dt,
                      exposure_end_dt = data$exposure_end_dt, 
                      enrollment_start_dt = data$ENRLMT_START_DT, 
                      enrollment_end_dt = data$ENRLMT_END_DT)
  } else {
    # Create intervals for each enrollment period
    enrollment_periods <- interval(data$ENRLMT_START_DT, data$ENRLMT_END_DT)
    
    # Create vectors for start and end dates
    start_dates <- int_start(enrollment_periods)
    end_dates <- int_end(enrollment_periods)
    
    # Initialize the list of enrollment periods
    enrollment_period <- list()
    
    # Initialize current interval
    current_start <- start_dates[1]
    current_end <- end_dates[1]
    
    # Loop through each interval
    for (i in 2:length(enrollment_periods)) {
      if (end_dates[i - 1] + days(1) == start_dates[i]) {
        # If current end + 1 day is the start of next interval, merge them
        current_end <- end_dates[i]
      } else {
        # Add current interval to the list and reset current interval
        enrollment_period <- append(enrollment_period, interval(current_start, current_end))
        current_start <- start_dates[i]
        current_end <- end_dates[i]
      }
    }
    
    # Add the last interval
    enrollment_period <- append(enrollment_period, interval(current_start, current_end))
    
    # Return result as a data.table
    out <- data.table(BENE_ID = data$BENE_ID[1], 
                      washout_start_dt = data$washout_start_dt[1], 
                      msk_diagnosis_dt = data$msk_diagnosis_dt[1],
                      exposure_end_dt = data$exposure_end_dt[1], 
                      enrollment_start_dt = as.Date(int_start(enrollment_period)), 
                      enrollment_end_dt = as.Date(int_end(enrollment_period)))
  }

  out[(washout_start_dt >= enrollment_start_dt)] # must be enrolled for 6 month washout
}

# # Test case
# tmp <- readRDS(files[1])
# find_enrollment_periods(tmp$`HHHHHH447AkdkHd`)
# find_enrollment_periods(tmp[[100]])

plan(multisession, workers = 20)

for (i in seq_along(files)) {
  tmp <- readRDS(files[i])
  
  valid_periods <- 
    foreach(x = tmp, .combine = "rbind", .options.future = list(chunk.size = 5e3)) %dofuture% {
      find_enrollment_periods(x)
    }
  
  write_data(
    valid_periods, 
    paste0("enrollment_period_chunk_", i, ".fst"), 
    "/mnt/general-data/disability/everything-local-lmtp/valid_enrollment_periods"
  )
}

rm(valid_periods)
gc()

plan(sequential)

cohort <- 
  "/mnt/general-data/disability/everything-local-lmtp/valid_enrollment_periods" |> 
  list.files(full.names = TRUE) |> 
  lapply(\(x) read_fst(x, columns = "BENE_ID", as.data.table = TRUE)) |> 
  rbindlist()

washout <- merge(washout, cohort)

# export
write_data(washout, "msk_washout_continuous_enrollment_dts.fst")
