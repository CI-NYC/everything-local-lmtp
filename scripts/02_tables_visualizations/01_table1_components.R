# READ ME -----------------------------------------------------------------
#
# Author: Shodai Inose
# Created: 2024-10-02
# Last updated:
#
#
# -------------------------------------------------------------------------

# Set up -----------------------------------------------------------------------

# Load libraries
library(tidylog)
library(tidyverse)
library(janitor)
library(arrow)
library(gtsummary)
library(gt)
library(data.table)

drv_root <- "/mnt/general-data/disability/everything-local-lmtp/"


analysis_cohort <- fst::read_fst(paste0(drv_root ,"msk_cohort_with_MH.fst")) |>
  as.data.table() |>
  mutate(conditional_group = case_when(subset_B3 == "TRUE" ~ 4,
                                       subset_B2 == "TRUE" ~ 3,
                                       subset_B1 == "TRUE" ~ 2,
                                       subset_B3 == "FALSE" & subset_B2 == "FALSE" & subset_B1 == "FALSE" ~ 1))

num_opioids <- readRDS("/mnt/general-data/disability/everything-local-lmtp/num_opioids.rds")

analysis_cohort <- analysis_cohort |>
  left_join(num_opioids)

# Create data for table 1 pieces -----------------------------------------------


dem_tbl_data <- analysis_cohort |>
  select(dem_age,
         dem_sex,
         dem_race,
         dem_primary_language_english,
         dem_married_or_partnered,
         dem_probable_high_income,
         dem_household_size,
         dem_veteran,
         dem_tanf_benefits,
         dem_ssi_benefits,
         conditional_group) |>
  labelled::set_variable_labels(
    dem_age = "Age",
    dem_sex = "Sex",
    dem_race = "Race/Ethnicity",
    dem_primary_language_english = "Primary Language English",
    dem_married_or_partnered = "Married/Partnered",
    dem_probable_high_income = "Probable High Income",
    dem_household_size = "Household Size",
    dem_veteran = "Veteran",
    dem_tanf_benefits = "TANF Benefits",
    dem_ssi_benefits = "SSI Benefits"
  ) 

comorb_tbl_data <- analysis_cohort |>
  select(bipolar_washout_cal,
         anxiety_washout_cal,
         adhd_washout_cal,
         depression_washout_cal,
         mental_ill_washout_cal,
         conditional_group
  ) |>
  labelled::set_variable_labels(
    bipolar_washout_cal = "Bipolar",
    anxiety_washout_cal = "Anxiety",
    adhd_washout_cal = "ADD/ADHD",
    depression_washout_cal = "Depression",
    mental_ill_washout_cal = "Other Mental Illness")

exp_cond_tbl_data <- analysis_cohort  |>
  select(exposure_max_daily_dose_mme,
         exposure_days_supply,
         num_opioids,
         conditional_group) |>
  labelled::set_variable_labels(
    exposure_max_daily_dose_mme = "Exposure Maximum Daily MME Dose",
    exposure_max_daily_dose_mme = "Exposure Days Supply",
    num_opioids = "Number of Opioid Prescriptions"
  )

oud_tbl_data <- analysis_cohort  |>
  mutate(across(starts_with("cens"), ~ ifelse(. == 1, 0, ifelse(. == 0, 1, .)))) |>
  select(oud_period_1,
         cens_period_1,
         oud_period_2,
         cens_period_2,
         oud_period_3,
         cens_period_3,
         oud_period_4,
         cens_period_4,
         oud_period_5,
         cens_period_5,
         oud_hillary_period_1,
         cens_hillary_period_1,
         oud_hillary_period_2,
         cens_hillary_period_2,
         oud_hillary_period_3,
         cens_hillary_period_3,
         oud_hillary_period_4,
         cens_hillary_period_4,
         oud_hillary_period_5,
         cens_hillary_period_5,
         conditional_group) |>
  labelled::set_variable_labels(
    oud_period_1 = "OUD by Period 1",
    cens_period_1 = "Cens by Period 1",
    oud_period_2 = "OUD by Period 2",
    cens_period_2 = "Cens by Period 2",
    oud_period_3 = "OUD by Period 3",
    cens_period_3 = "Cens by Period 3",
    oud_period_4 = "OUD by Period 4",
    cens_period_4 = "Cens by Period 4",
    oud_period_5 = "OUD by Period 5",
    cens_period_5 = "Cens by Period 5"
  )

## Demographics
dem_tbl <- dem_tbl_data |>
  tbl_summary(missing = "ifany", by = conditional_group) |>
  bold_labels()

latex_dem_tbl <- dem_tbl |>
  as_gt() |>
  as_latex()

dem_tbl_df <- dem_tbl |>
  as_gt() |>
  as.data.frame() |>
  select(label:stat_4)


## Comorbidities
comorb_tbl <- comorb_tbl_data |>
  tbl_summary(missing = "ifany", by = conditional_group) |>
  bold_labels()

latex_comorb_tbl <- comorb_tbl |>
  as_gt() |>
  as_latex()

comorb_tbl_df <- comorb_tbl |>
  as_gt() |>
  as.data.frame() |>
  select(label:stat_4)

## Exposure
exp_tbl <- exp_cond_tbl_data |>
  tbl_summary(missing = "ifany", by = conditional_group) |>
  bold_labels()

latex_exp_tbl <- exp_tbl |>
  as_gt() |>
  as_latex()

exp_tbl_df <- exp_tbl |>
  as_gt() |>
  as.data.frame() |>
  select(label:stat_4)

## OUD

oud_tbl <- oud_tbl_data |>
  tbl_summary(missing = "ifany", by = conditional_group) |>
  bold_labels()

latex_oud_tbl <- oud_tbl |>
  as_gt() |>
  as_latex()

oud_tbl_df <- oud_tbl |>
  as_gt() |>
  as.data.frame() |>
  select(label:stat_4)


# Print out Latex code ---------------------------------------------------------

# Full cohort
cat(latex_dem_tbl)
cat(latex_comorb_tbl)
cat(latex_exp_tbl)
cat(latex_oud_tbl)
