# -------------------------------------
# Script: 07_combine_exclusions_exposure_outcome.R
# Author: Nick Williams
# Purpose: Combine exclusion/inclusion criteria, exposure, outcome and censoring files.
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)

source("R/helpers.R")

# base cohort
cohort <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst")
# debse exclusions
debse_exclusions <- load_data("msk_washout_continuous_enrollment_opioid_requirements_tafdebse_exclusions.fst")
# iph exclusions
iph_exclusions <- load_data("msk_washout_continuous_enrollment_opioid_requirements_tafiph_exclusions.fst")
# oth exclusions
oth_exclusions <- load_data("msk_washout_continuous_enrollment_opioid_requirements_tafoth_exclusions.fst")
# oud exclusions
oud_exclusions <- load_data("msk_washout_continuous_enrollment_opioid_requirements_oud_exclusion.fst")
# exposures
exposures <- load_data("exposures_with_subsets.fst")
# censoring
cens <- load_data("msk_washout_continuous_enrollment_opioid_requirements_censoring.fst")
# outcomes
outcomes <- load_data("msk_washout_continuous_enrollment_opioid_requirements_oud_outcomes.fst")

cohort <- list(
  cohort, 
  debse_exclusions, 
  iph_exclusions, 
  oth_exclusions, 
  oud_exclusions
) |> 
  reduce(join, how = "left") |>
  mutate(across(everything(), ~ replace_na(., 0)))

nrow(cohort)

# exclusion MD
cohort |>
  filter(exclusion_maryland == 1) |>
  nrow()

cohort <- cohort |>
  filter(exclusion_maryland == 0)

# exclusion age
cohort |>
  filter(exclusion_age == 1) |>
  nrow()

cohort <- cohort |>
  filter(exclusion_age == 0)

# exclusion double birthday
cohort |>
  filter(exclusion_double_bdays == 1) |>
  nrow()

cohort <- cohort |>
  filter(exclusion_double_bdays == 0)

# exclusion pregnancy
cohort |>
  filter(exclusion_pregnancy == 1) |>
  nrow()

cohort <- cohort |>
  filter(exclusion_pregnancy == 0)

# exclusion unknown sex
cohort |>
  filter(exclusion_missing_sex == 1) |>
  nrow()

cohort <- cohort |>
  filter(exclusion_missing_sex == 0)

nrow(cohort)

# exclusion dual eligible
cohort |>
  filter(exclusion_dual_eligible == 1) |>
  nrow()

cohort <- cohort |>
  filter(exclusion_dual_eligible == 0)

nrow(cohort)


# exclusion cancer
cohort |>
  filter(exclusion_cancer == 1 | exclusion_cancer_oth == 1 | exclusion_cancer_iph == 1) |>
  nrow()

cohort <- cohort |>
  filter(exclusion_cancer == 0,
         exclusion_cancer_oth == 0,
         exclusion_cancer_iph == 0)

nrow(cohort)

# exclusion institution or pallative
cohort |>
  filter(exclusion_pall_iph == 1 | exclusion_pall_oth == 1 | exclusion_institution == 1) |>
  nrow()

cohort <- cohort |>
  filter(exclusion_pall_iph == 0,
         exclusion_pall_oth == 0,
         exclusion_institution == 0)

nrow(cohort)

# exclusion oud
cohort |>
  filter(exclusion_oud == 1) |>
  nrow()

cohort <- cohort |>
  filter(exclusion_oud == 0)

nrow(cohort)


