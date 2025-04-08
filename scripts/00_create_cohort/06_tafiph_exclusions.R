# -------------------------------------
# Script: 06_tafiph_exclusions.R
# Author: Nick Williams
# Updated: Shodai Inose (Mach 25 2025) change washout end date
# Purpose: Create exclusions based on TAFIPH files
# Notes: Modified from https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/03_initial_cohort_exclusions/clean_tafihp.R
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(fst)
library(collapse)
library(yaml)

source("R/helpers.R")

set.seed(9)

# Source ICD codes from the disability and chronic pain paper
codes <- read_yaml("data/public/icd_codes.yml")

# Read in IPH dataset
iph <- open_iph()

cohort <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst")

icd <-
  iph |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
  collect()

icd_washout <- 
  join(fselect(cohort, BENE_ID, washout_start_dt, washout_end_dt), 
     icd, 
     how = "inner") |> 
  fmutate(SRVC_BGN_DT = fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)) |> 
  fsubset(SRVC_BGN_DT %within% interval(washout_start_dt, washout_end_dt)) |> 
  as_tibble()

# Identify whether exclusion ICD code of interest occurs in washout ICDs
icd_exclusions <-
  icd_washout |>
  mutate(
    exclusion_pall_iph   = if_any(starts_with("DGNS_CD"),  ~ . %in% codes$palliative_care),
    exclusion_cancer_iph = if_any(starts_with("DGNS_CD"),  ~ . %in% codes$cancer)
  ) |> 
  select(BENE_ID, starts_with("exclusion")) |> 
  group_by(BENE_ID) |>
  summarize(across(starts_with("exclusion"), ~ ifelse(sum(.x) >= 1, 1, 0)))

icd_exclusions <- 
  join(cohort, icd_exclusions, how = "left") |> 
  mutate(across(starts_with("exclusion"), ~ replace_na(.x))) |> 
  fselect(BENE_ID, exclusion_pall_iph, exclusion_cancer_iph)

# export
write_data(icd_exclusions, "msk_washout_continuous_enrollment_opioid_requirements_tafiph_exclusions.fst")
