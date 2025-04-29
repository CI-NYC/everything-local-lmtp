# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(dplyr)

setwd("~/everything-local-lmtp")

source("R/helpers.R")

# reading in cohorts
cohort <- load_data("msk_washout_continuous_enrollment_opioid_requirements_with_exposures.fst") |>
  select(BENE_ID, min_opioid_date, exposure_period_end_dt)

cohort_7_day_gap <- load_data("msk_washout_continuous_enrollment_opioid_requirements_with_exposures_7_day_gap.fst") |>
  select(BENE_ID, min_opioid_date, exposure_period_end_dt) |>
  rename("exposure_period_end_dt_7_day_gap" = "exposure_period_end_dt")

all_enrollment_dates <- load_data(paste0("all_possible_enrollment_dates/combined_all_enrolled_dates_cohort_ALL", ".fst")) |>
  left_join(cohort) |>
  left_join(cohort_7_day_gap) |>
  filter(date >= min_opioid_date) # only need dates from post-exposure start

# creating exposure end date periods (each period is 91 days long)

all_enrollment_dates <- all_enrollment_dates |>
  mutate(period_1_end_dt = exposure_period_end_dt + days(91),
         period_2_end_dt = period_1_end_dt + days(91),
         period_3_end_dt = period_2_end_dt + days(91),
         period_4_end_dt = period_3_end_dt + days(91),
         period_5_end_dt = period_4_end_dt + days(91),
         period_1_end_dt_7_day_gap = exposure_period_end_dt_7_day_gap + days(91),
         period_2_end_dt_7_day_gap = period_1_end_dt_7_day_gap + days(91),
         period_3_end_dt_7_day_gap = period_2_end_dt_7_day_gap + days(91),
         period_4_end_dt_7_day_gap = period_3_end_dt_7_day_gap + days(91),
         period_5_end_dt_7_day_gap = period_4_end_dt_7_day_gap + days(91)
  )

# creating indicator for each period

all_enrollment_dates <- all_enrollment_dates |>
  mutate(enrolled_during_period_1 = case_when(date >= exposure_period_end_dt + days(1) & date <= period_1_end_dt & enrolled == 1 ~ 1,
                                              TRUE ~ 0),
         enrolled_during_period_2 = case_when(date >= period_1_end_dt + days(0) & date <= period_2_end_dt & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         enrolled_during_period_3 = case_when(date >= period_2_end_dt + days(0) & date <= period_3_end_dt & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         enrolled_during_period_4 = case_when(date >= period_3_end_dt + days(0) & date <= period_4_end_dt & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         enrolled_during_period_5 = case_when(date >= period_4_end_dt + days(0) & date <= period_5_end_dt & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         study_end_dt = exposure_period_end_dt + days(455),
         enrolled_during_study_end_dt = case_when(date == study_end_dt & enrolled == 1 ~ 1,
                                                  TRUE ~ 0),
         enrolled_during_period_1_7_day_gap = case_when(date >= exposure_period_end_dt_7_day_gap + days(1) & date <= period_1_end_dt_7_day_gap & enrolled == 1 ~ 1,
                                              TRUE ~ 0),
         enrolled_during_period_2_7_day_gap = case_when(date >= period_1_end_dt_7_day_gap + days(0) & date <= period_2_end_dt_7_day_gap & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         enrolled_during_period_3_7_day_gap = case_when(date >= period_2_end_dt_7_day_gap + days(0) & date <= period_3_end_dt_7_day_gap & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         enrolled_during_period_4_7_day_gap = case_when(date >= period_3_end_dt_7_day_gap + days(0) & date <= period_4_end_dt_7_day_gap & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         enrolled_during_period_5_7_day_gap = case_when(date >= period_4_end_dt_7_day_gap + days(0) & date <= period_5_end_dt_7_day_gap & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         study_end_dt_7_day_gap = exposure_period_end_dt_7_day_gap + days(455),
         enrolled_during_study_end_dt_7_day_gap = case_when(date == study_end_dt_7_day_gap & enrolled == 1 ~ 1,
                                                  TRUE ~ 0),
  )

all_enrollment_dates_grouped <- all_enrollment_dates |>
  group_by(BENE_ID) |>
  summarize(enrollment_cens_period_1 = any(enrolled_during_period_1 == 1),
            enrollment_cens_period_2 = any(enrolled_during_period_2 == 1),
            enrollment_cens_period_3 = any(enrolled_during_period_3 == 1),
            enrollment_cens_period_4 = any(enrolled_during_period_4 == 1),
            enrollment_cens_period_5 = any(enrolled_during_period_5 == 1),
            enrollment_cens_study_end_dt = any(enrolled_during_study_end_dt == 1)
            ) |>
  mutate(enrollment_cens_period_1 = case_when(enrollment_cens_period_1 == FALSE ~ 1, # disenrolled prior to start of outcome period
                                              enrollment_cens_period_2 == FALSE ~ 1, # not enrolled at all during next period, then they are censored during this period
                                              TRUE ~ 0), 
         enrollment_cens_period_2 =  case_when(enrollment_cens_period_1 == 1 ~ 1,
                                               enrollment_cens_period_3 == FALSE ~ 1, # not enrolled at all during next period, then they are censored during this period
                                               TRUE ~ 0), 
         enrollment_cens_period_3 =  case_when(enrollment_cens_period_1 == 1 ~ 1,
                                               enrollment_cens_period_2 == 1 ~ 1,
                                               enrollment_cens_period_4 == FALSE ~ 1, # not enrolled at all during next period, then they are censored during this period
                                               TRUE ~ 0),
         enrollment_cens_period_4 =  case_when(enrollment_cens_period_1 == 1 ~ 1,
                                               enrollment_cens_period_2 == 1 ~ 1,
                                               enrollment_cens_period_3 == 1 ~ 1,
                                               enrollment_cens_period_5 == FALSE ~ 1, # not enrolled at all during next period, then they are censored during this period
                                               TRUE ~ 0), 
         enrollment_cens_period_5 =  case_when(enrollment_cens_period_1 == 1 ~ 1,
                                               enrollment_cens_period_2 == 1 ~ 1,
                                               enrollment_cens_period_3 == 1 ~ 1,
                                               enrollment_cens_period_4 == 1 ~ 1,
                                               enrollment_cens_study_end_dt == FALSE ~ 1, # still enrolled during period 5 but not by end of study, then they are censored during this period
                                               TRUE ~ 0), 
    
  ) |>
  select(-enrollment_cens_study_end_dt)

write_data(all_enrollment_dates_grouped, paste0("all_possible_enrollment_dates/censoring_enrollment", ".fst"))

rm(all_enrollment_dates_grouped)

all_enrollment_dates_grouped_7_day_gap <- all_enrollment_dates |>
  group_by(BENE_ID) |>
  summarize(enrollment_cens_period_1 = any(enrolled_during_period_1_7_day_gap == 1),
            enrollment_cens_period_2 = any(enrolled_during_period_2_7_day_gap == 1),
            enrollment_cens_period_3 = any(enrolled_during_period_3_7_day_gap == 1),
            enrollment_cens_period_4 = any(enrolled_during_period_4_7_day_gap == 1),
            enrollment_cens_period_5 = any(enrolled_during_period_5_7_day_gap == 1),
            enrollment_cens_study_end_dt = any(enrolled_during_study_end_dt == 1)
  ) |>
  mutate(enrollment_cens_period_1 = case_when(enrollment_cens_period_1 == FALSE ~ 1, # disenrolled prior to start of outcome period
                                              enrollment_cens_period_2 == FALSE ~ 1, # not enrolled at all during next period, then they are censored during this period
                                              TRUE ~ 0), 
         enrollment_cens_period_2 =  case_when(enrollment_cens_period_1 == 1 ~ 1,
                                               enrollment_cens_period_3 == FALSE ~ 1, # not enrolled at all during next period, then they are censored during this period
                                               TRUE ~ 0), 
         enrollment_cens_period_3 =  case_when(enrollment_cens_period_1 == 1 ~ 1,
                                               enrollment_cens_period_2 == 1 ~ 1,
                                               enrollment_cens_period_4 == FALSE ~ 1, # not enrolled at all during next period, then they are censored during this period
                                               TRUE ~ 0),
         enrollment_cens_period_4 =  case_when(enrollment_cens_period_1 == 1 ~ 1,
                                               enrollment_cens_period_2 == 1 ~ 1,
                                               enrollment_cens_period_3 == 1 ~ 1,
                                               enrollment_cens_period_5 == FALSE ~ 1, # not enrolled at all during next period, then they are censored during this period
                                               TRUE ~ 0), 
         enrollment_cens_period_5 =  case_when(enrollment_cens_period_1 == 1 ~ 1,
                                               enrollment_cens_period_2 == 1 ~ 1,
                                               enrollment_cens_period_3 == 1 ~ 1,
                                               enrollment_cens_period_4 == 1 ~ 1,
                                               enrollment_cens_study_end_dt == FALSE ~ 1, # still enrolled during period 5 but not by end of study, then they are censored during this period
                                               TRUE ~ 0), 
         
  ) |>
  select(-enrollment_cens_study_end_dt)

write_data(all_enrollment_dates_grouped_7_day_gap, paste0("all_possible_enrollment_dates/censoring_enrollment", "_7_day_gap", ".fst"))











