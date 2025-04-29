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
library(future)
library(furrr)
library(doParallel)
options(cores = 30)
registerDoParallel()
plan(multicore)
getDoParWorkers()


setwd("~/everything-local-lmtp")

source("R/helpers.R")

# Load washout dates
washout <- load_data("msk_washout_opioid_requirements.fst")

# number of people at start
start_n <- washout |> nrow()

# Load temporary files for 01_01_filter_continuous_enrollment.R
files <- 
  "/mnt/general-data/disability/everything-local-lmtp/tmp" |> 
  list.files(full.names = TRUE)

# # Test case
# tmp <- readRDS(files[1])
# find_enrollment_periods(tmp$`HHHHHH447AkdkHd`)
# find_enrollment_periods(tmp[[100]])

# get BENE_ID of cohort
washout <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst") |>
  select(BENE_ID, min_opioid_date)

washout_ID <- washout$BENE_ID

registerDoFuture()

results_tmp <- list()
for (i in 8:8) {
  tmp <- readRDS(files[i])
  
  # only keep those in the cohort
  tmp <- tmp[names(tmp) %in% washout_ID]

  final_df <- foreach(z = seq_along(tmp), .combine = 'rbind', .options.future = list(chunk.size = 5e3)) %dopar% {
    # TEST: df <- tmp$HHHHHH44eAAe4Ce
    # df <- tmp$HHHHHH4477d4Bnd
    df <- tmp[[z]] # for each individual 
    
    BENE_ID <- df[1]$BENE_ID[1]
    
    min_start_date <- min(df$ENRLMT_START_DT, na.rm = TRUE) # earliest possible start date
    max_end_date <- max(df$ENRLMT_END_DT, na.rm = TRUE) # latest possible end date
    all_possible_enrollment_dates <- seq.Date(from = as.Date(min_start_date), to = as.Date(max_end_date), by = "day") # all possible enrollment dates for individual z
    
    # make all possible enrolled dates into a dataframe
    all_possible_enrollment_dates_df <- data.frame(date = all_possible_enrollment_dates)
    
    # for each row (enrollment period), make all possible enrollment dates, then combine across rows and get distinct dates
    enrolled_dates_df <- do.call(rbind, apply(df, 1, function(row) {
      dates <- seq.Date(from = as.Date(row["ENRLMT_START_DT"]), to = as.Date(row["ENRLMT_END_DT"]), by = "day")
      data.frame(date = dates) 
    })) |>
      distinct() |>
      mutate(enrolled = 1)
    
    all_possible_enrollment_dates_df <- all_possible_enrollment_dates_df |>
      left_join(enrolled_dates_df) |>
      mutate(enrolled = case_when(is.na(enrolled) ~ 0,
                       TRUE ~ enrolled)) |>
      mutate(BENE_ID = BENE_ID) |>
      relocate(BENE_ID, .before = date)
    
  }
  
  write_data(
    final_df,
    paste0("/mnt/general-data/disability/everything-local-lmtp/all_possible_enrollment_dates/combined_all_enrolled_dates_cohort", i, ".fst")
  )
  
  # storing dataframe into list
  #results_tmp[[i]] <- final_df
}

# combining all 8 lists into one dataframe
# combined_df <- bind_rows(my_list)
# 
# # only get those in cohort
# washout <- washout |>
#   left_join(combined_df) |>
#   filter(date >= min_opioid_date) # only care about dates post-washout


# write_data(
#   washout,
#   "/mnt/general-data/disability/everything-local-lmtp/all_possible_enrollment_dates/combined_all_enrolled_dates_cohort.fst"
# )
