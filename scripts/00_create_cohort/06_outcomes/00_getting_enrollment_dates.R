# -------------------------------------
# Script: 00_getting_enrollment_dates.R
# Author: Shodai Inose
# Purpose: Create a dataframe for all people in cohort containing every possible date of enrollment and an indicator 
#   for whether or not they were enrolled on that date.
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
#library(doFuture)
library(dplyr)
# library(future)
# library(furrr)
# library(doParallel)
# options(cores = 30)
# registerDoParallel()
# plan(multicore)
# getDoParWorkers()


setwd("~/everything-local-lmtp")

source("R/helpers.R")

# Load temporary files from 01_01_filter_continuous_enrollment.R
files <- 
  "/mnt/general-data/disability/everything-local-lmtp/tmp_post_exposure" |> 
  list.files(full.names = TRUE)

# # Test case
# tmp <- readRDS(files[1])
# find_enrollment_periods(tmp$`HHHHHH447AkdkHd`)
# find_enrollment_periods(tmp[[100]])

# get BENE_ID of cohort
washout <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst") |>
  select(BENE_ID, min_opioid_date)

washout_ID <- washout$BENE_ID

# gets all possble enrollment dates (using the minimum start and maximum end date) then creates an indicator for whether or not they were enrolled on that date
getdates <- function(df) {
  # TEST: df <- tmp$HHHHHH44eAAe4Ce
  # df <- tmp$HHHHHH4477d4Bnd

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
                                TRUE ~ enrolled))
  
  all_possible_enrollment_dates_df
}

# applying function to each chunk
for (i in seq_along(files)
     ) {
  tmp <- readRDS(files[i])
  
  # only keep those in the cohort
  #tmp <- tmp[names(tmp) %in% washout_ID]
  
  # comine into 1 large df
  tmp_df <- bind_rows(tmp)
  
  # get dates for each beneficiary and return as a df
  final_df <- tmp_df |>
    group_by(BENE_ID) |>
    group_map(~ {
      output <- getdates(.x)
      bind_cols(.y, output)
    }) |>
    bind_rows()
  
  write_data(
    final_df,
    paste0("all_possible_enrollment_dates/combined_all_enrolled_dates_cohort_", i, ".fst")
  )
}

# # combining results into a list
results_list <- list()
for (i in seq_along(files)){
  final_df <- load_data(paste0("all_possible_enrollment_dates/combined_all_enrolled_dates_cohort_", i, ".fst"))

  results_list[[i]] <- final_df
}
# 
# # combining list into a dataframe
combined_all_df <- bind_rows(results_list) |>
  filter(BENE_ID %in% washout_ID)

write_data(
  combined_all_df,
  paste0("all_possible_enrollment_dates/combined_all_enrolled_dates_cohort_ALL", ".fst")
)