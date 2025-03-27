# -------------------------------------
# Script: 06_tafdebse_exclusions.R
# Author: Nick Williams
# Updated: Shodai Inose (Mach 25 2025) change washout end date
# Purpose:
# Notes: Modified from https://github.com/CI-NYC/disability-chronic-pain/blob/main/scripts/02_clean_tafdebse.R
# -------------------------------------

library(collapse)
library(fst)
library(arrow)
library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)
library(yaml)
library(purrr)
library(stringr)

source("R/helpers.R")

cohort <- load_data("msk_washout_continuous_enrollment_opioid_requirements.fst")

codes <- read_yaml("data/public/eligibility_codes.yml")

# Load demographics dataset
demo <- open_demo()

demo <- right_join(demo, cohort) |> 
  collect()

# exclude maryland --------------------------------------------------------

exclusion_md <- 
  fselect(demo, BENE_ID, washout_start_dt, RFRNC_YR, STATE_CD) |> 
  fsubset(year(washout_start_dt) == as.numeric(RFRNC_YR)) |> 
  fmutate(exclusion_maryland = as.numeric("MD" == STATE_CD)) |> 
  fselect(BENE_ID, exclusion_maryland) |> 
  funique()

exclusion_md <- fselect(cohort, BENE_ID) |> 
  join(exclusion_md, how = "left")

# age ---------------------------------------------------------------------

# Remove observations with more than 1 birthdate? 
exclusion_age <- 
  fselect(demo, BENE_ID, washout_start_dt, BIRTH_DT) |> 
  funique() |> 
  drop_na() |> 
  fmutate(age_enrollment = floor(time_length(interval(BIRTH_DT, washout_start_dt), "years")), 
          exclusion_age = fcase(age_enrollment < 19, 1, 
                                age_enrollment >= 64, 1, 
                                default = 0)) |> 
  group_by(BENE_ID) |> 
  add_tally() |> 
  fmutate(exclusion_double_bdays = ifelse(n > 1, 1, 0)) |> 
  fselect(BENE_ID, exclusion_age, exclusion_double_bdays)

exclusion_age <- fselect(cohort, BENE_ID) |> 
  join(exclusion_age, how = "left")

# sex ---------------------------------------------------------------------

exclusion_sex <- 
  fselect(demo, BENE_ID, SEX_CD) |> 
  funique() |> 
  fmutate(exclusion_missing_sex = as.numeric(is.na(SEX_CD)), 
          .keep = c("BENE_ID", "exclusion_missing_sex"))
  
exclusion_sex <- fselect(cohort, BENE_ID) |> 
  join(exclusion_sex, how = "left")

# eligibility codes -------------------------------------------------------

eligibility_codes <- 
  select(demo, BENE_ID, RFRNC_YR, starts_with("ELGBLTY_GRP_CD"), -ELGBLTY_GRP_CD_LTST) |>
  pivot(ids = c("BENE_ID", "RFRNC_YR"), 
        how = "l", 
        names = list("month", "code")) |> 
  drop_na() |> 
  fmutate(month = str_extract(month, "\\d+$"), 
          year = as.numeric(RFRNC_YR),
          elig_dt = as.Date(paste0(year, "-", month, "-01"))) |> 
  join(cohort, how = "inner") |> 
  fselect(BENE_ID, washout_start_dt, washout_end_dt, code, elig_dt)

eligibility_codes <- 
  fsubset(eligibility_codes, elig_dt %within% interval(washout_start_dt, washout_end_dt))

# Filter to last eligiblity code in washout time period
wo_eligibility_codes <- 
  roworder(eligibility_codes, elig_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == n())

exclusion_codes <- 
  fmutate(wo_eligibility_codes,
          exclusion_pregnancy = code %in% codes$pregnant, 
          exclusion_institution = code %in% codes$institution, 
          exclusion_cancer = code %in% codes$cancer, 
          exclusion_dual_eligible_1 = code %in% codes$dual_eligibility,
          probable_high_income_cal = code %in% codes$income) |> 
  mutate(across(c(starts_with("exclusion"), probable_high_income_cal), as.numeric))

exclusion_codes <- 
  fselect(cohort, BENE_ID) |> 
  join(exclusion_codes, how = "left") |> 
  mutate(across(c(starts_with("exclusion"), probable_high_income_cal) , ~ replace_na(.x))) 

income_codes <- exclusion_codes |>
  fselect(BENE_ID, probable_high_income_cal)

write_data(income_codes, "probable_high_income_cal.fst")

exclusion_codes <- exclusion_codes |> 
  fselect(BENE_ID, exclusion_pregnancy, exclusion_institution, exclusion_cancer, exclusion_dual_eligible_1)

# dual eligibility --------------------------------------------------------

dual_codes <- 
  select(demo, BENE_ID, RFRNC_YR, starts_with("DUAL_ELGBL_CD"), -DUAL_ELGBL_CD_LTST) |> 
  pivot(ids = c("BENE_ID", "RFRNC_YR"), 
        how = "l", 
        names = list("month", "code"), 
        na.rm = TRUE) |> 
  fsubset(code %!=% "00") |> 
  fmutate(month = str_extract(month, "\\d+$"), 
          year = as.numeric(RFRNC_YR),
          elig_dt = as.Date(paste0(year, "-", month, "-01"))) |> 
  fselect(BENE_ID, code, elig_dt)

exclusion_dual_eligible <- 
  join(cohort, dual_codes, how = "inner") |> 
  fmutate(exclusion_dual_eligible = elig_dt %within% interval(washout_start_dt, washout_end_dt)) |> 
  fsubset(exclusion_dual_eligible) |> 
  fselect(BENE_ID, exclusion_dual_eligible) |> 
  funique() |> 
  join(fselect(cohort, BENE_ID), how = "right") |> 
  fmutate(exclusion_dual_eligible = replace_na(as.numeric(exclusion_dual_eligible)))

# join --------------------------------------------------------------------

exclusions <- 
  list(exclusion_md,
       exclusion_age, 
       exclusion_sex, 
       exclusion_codes, 
       exclusion_dual_eligible) |> 
  reduce(join)

exclusions <- 
  fmutate(exclusions, 
          exclusion_dual_eligible = 
            as.numeric((exclusion_dual_eligible_1 + exclusion_dual_eligible) >= 1)) |> 
  fselect(-exclusion_dual_eligible_1)

write_data(exclusions, "msk_washout_continuous_enrollment_opioid_requirements_tafdebse_exclusions.fst")
