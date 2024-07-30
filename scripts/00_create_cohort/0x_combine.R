# -------------------------------------
# Script: 0x_combine.R
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
  reduce(join, how = "left")

# Remove observations with exclusions
cohort <- filter(cohort, if_all(starts_with("exclusion"), \(x) x == 0))

# Add in exposure, outcome, and censoring data
cohort <- 
  join(cohort, exposures, how = "left") |> 
  join(outcomes, how = "left") |> 
  join(cens, how = "left")

convert_cens_to_na <- function (data, outcomes, cens) {
  DT <- as.data.table(data)
  tau <- length(outcomes)
  for (j in 1:(tau - 1)) {
    modify <- setdiff(cens[match(cens[j], cens):tau], cens[j])
    outcome_j <- outcomes[j]
    DT[get(outcome_j) == 1, `:=`((modify), lapply(.SD, function(x) NA_real_)), .SDcols = modify]
  }
  DT[]
  DT
}

convert_outcome_to_na <- function (data, outcomes, cens) {
  DT <- as.data.table(data)
  tau <- length(outcomes)
  for (j in 1:(tau - 1)) {
    modify <- outcomes[match(outcomes[j], outcomes):tau]
    cens_j <- cens[j]
    DT[get(cens_j) == 0, `:=`((modify), lapply(.SD, function(x) NA_real_)), .SDcols = modify]
  }
  DT[]
  DT
}

cohort <- 
  convert_cens_to_na(cohort, paste0("oud_period_", 1:5), paste0("cens_period_", 1:5)) |> 
  convert_outcome_to_na(paste0("oud_period_", 1:5), paste0("cens_period_", 1:5)) |> 
  select(BENE_ID, washout_start_dt, msk_diagnosis_dt,
         starts_with("exposure"), starts_with("subset"), 
         cens_period_1, oud_period_1, 
         cens_period_2, oud_period_2, 
         cens_period_3, oud_period_3, 
         cens_period_4, oud_period_4, 
         cens_period_5, oud_period_5)

write_fst(
  cohort, 
  file.path(
    "/mnt/general-data/disability/everything-local-lmtp", 
    "inclusion_exclusion_cohort_with_exposure_outcomes.fst"
  )
)
