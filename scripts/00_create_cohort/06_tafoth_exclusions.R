# -------------------------------------
# Script: 06_tafoth_exclusions.R
# Author: Nick Williams
# Updated: Shodai Inose (Mach 25 2025) change washout end date
# Purpose: 
# Notes: Modified from https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/03_initial_cohort_exclusions/clean_tafoth.R
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(collapse)
library(data.table)
library(fst)
library(yaml)

source("R/helpers.R")

set.seed(9)

cohort <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst")

# Load icd codes
codes <- read_yaml("data/public/icd_codes.yml")

# Read in OTH dataset
oth <- open_oth()

oth <- 
  select(oth, BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |> 
  inner_join(
    select(cohort, BENE_ID, washout_start_dt, washout_end_dt), 
    by = "BENE_ID"
  ) |> 
  collect() |> 
  fsubset(!(is.na(DGNS_CD_1) & is.na(DGNS_CD_2))) |> 
  fmutate(SRVC_BGN_DT = fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)) |>
  fsubset(SRVC_BGN_DT %within% interval(washout_start_dt, washout_end_dt))

# Identify whether exclusion ICD code of interest occurs in washout ICDs
oth_exclusions <-
  mutate(
    oth, 
    exclusion_pall_oth = +(if_any(starts_with("DGNS_CD"), ~. %in% codes$palliative_care)),
    exclusion_cancer_oth = +(if_any(contains("DGNS_CD"), ~. %in% codes$cancer)) 
  ) |> 
  fselect(BENE_ID, exclusion_pall_oth, exclusion_cancer_oth)

# keep only one row per beneficiary
oth_exclusions <-
  group_by(oth_exclusions, BENE_ID) |>
  summarize(across(exclusion_pall_oth:exclusion_cancer_oth, ~ fifelse(sum(.x) >= 1, 1, 0)))  |>
  ungroup()

# export
write_data(oth_exclusions, "msk_washout_continuous_enrollment_opioid_requirements_tafoth_exclusions.fst")
