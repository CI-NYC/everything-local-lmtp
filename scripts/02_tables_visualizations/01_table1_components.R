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
                                       subset_B3 == "FALSE" & subset_B2 == "FALSE" & subset_B1 == "FALSE" ~ 1)) |>
  mutate(subset_B4 = ifelse(exposure_max_daily_dose_mme >= 90, TRUE, FALSE),
         subset_B5 = ifelse(exposure_max_daily_dose_mme >= 90 & exposure_days_supply > 7, TRUE, FALSE),
         subset_B6 = ifelse(exposure_days_supply > 30, TRUE, FALSE),
         subset_B7 = ifelse(exposure_days_supply > 30 & exposure_max_daily_dose_mme >= 50, TRUE, FALSE),
         subset_B8 = ifelse(exposure_days_supply > 30 & exposure_max_daily_dose_mme >= 90, TRUE, FALSE)
  ) |>
  mutate(subset_B_under_20 = ifelse(exposure_max_daily_dose_mme <= 20, TRUE, FALSE),
         subset_B_days_7_dose_under_20 = ifelse(exposure_days_supply <= 7 & exposure_max_daily_dose_mme <= 20, TRUE, FALSE),
         subset_B_not_risky_days = ifelse(exposure_days_supply <= 7, TRUE, FALSE),
         subset_cohort = TRUE
  )

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
         subset_B1,
         subset_B2,
         subset_B3,
         subset_B4,
         subset_B5,
         subset_B6,
         subset_B7,
         subset_B8,
         subset_B_under_20,
         subset_B_days_7_dose_under_20,
         subset_B_not_risky_days,
         subset_cohort) |>
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
         subset_B1,
         subset_B2,
         subset_B3,
         subset_B4,
         subset_B5,
         subset_B6,
         subset_B7,
         subset_B8,
         subset_B_under_20,
         subset_B_days_7_dose_under_20,
         subset_B_not_risky_days,
         subset_cohort) |>
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
         subset_B1,
         subset_B2,
         subset_B3,
         subset_B4,
         subset_B5,
         subset_B6,
         subset_B7,
         subset_B8,
         subset_B_under_20,
         subset_B_days_7_dose_under_20,
         subset_B_not_risky_days,
         subset_cohort) |>
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
         subset_B1,
         subset_B2,
         subset_B3,
         subset_B4,
         subset_B5,
         subset_B6,
         subset_B7,
         subset_B8,
         subset_B_under_20,
         subset_B_days_7_dose_under_20,
         subset_B_not_risky_days,
         subset_cohort) |>
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
dem_tbl_cohort <- dem_tbl_data |>
  filter(subset_cohort == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days#,
            #subset_cohort
  )) |>
  tbl_summary(missing = "ifany",
              include = -"subset_cohort") |>
  bold_labels() |>
  modify_header(label ~ "**Cohort**")

dem_tbl_subset_B_under_20 <- dem_tbl_data |>
  filter(subset_B_under_20 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            #subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_under_20) |>
  bold_labels() |>
  modify_header(label ~ "**MME <= 20**")

dem_tbl_B1 <- dem_tbl_data |>
  filter(subset_B1 == TRUE) |>
  select(-c(#subset_B1,
    subset_B2,
    subset_B3,
    subset_B4,
    subset_B5,
    subset_B6,
    subset_B7,
    subset_B8,
    subset_B_under_20,
    subset_B_days_7_dose_under_20,
    subset_B_not_risky_days,
    subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B1) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50**")

dem_tbl_B4 <- dem_tbl_data |>
  filter(subset_B4 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            #subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B4) |>
  bold_labels() |>
  modify_header(label ~ "**MME >= 90**")

dem_tbl_subset_B_not_risky_days <- dem_tbl_data |>
  filter(subset_B_not_risky_days == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            #subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_not_risky_days) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days <= 7**")

dem_tbl_B2 <- dem_tbl_data |>
  filter(subset_B2 == TRUE) |>
  select(-c(subset_B1,
            #subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B2) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days > 7**")

dem_tbl_B6 <- dem_tbl_data |>
  filter(subset_B6 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            #subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B6) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days > 30**")

dem_tbl_subset_B_days_7_dose_under_20 <- dem_tbl_data |>
  filter(subset_B_days_7_dose_under_20 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            #subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_days_7_dose_under_20) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME <= 50, Days <= 7**")

dem_tbl_B3 <- dem_tbl_data |>
  filter(subset_B3 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            #subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B3) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50, Days > 7**")

dem_tbl_B5 <- dem_tbl_data |>
  filter(subset_B5 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            #subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B5) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 90, Days > 7**")

dem_tbl_B7 <- dem_tbl_data |>
  filter(subset_B7 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            #subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B7) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50, Days > 30**")

dem_tbl_B8 <- dem_tbl_data |>
  filter(subset_B8 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            #subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B8) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 90, Days > 30**")

dem_tbl <- tbl_merge(
  tbls = list(dem_tbl_cohort, dem_tbl_subset_B_under_20, dem_tbl_B1, dem_tbl_B4,
              dem_tbl_subset_B_not_risky_days, dem_tbl_B2, dem_tbl_B6, 
              dem_tbl_subset_B_days_7_dose_under_20, dem_tbl_B3, dem_tbl_B5, dem_tbl_B7, dem_tbl_B8),
  tab_spanner = c("**Cohort**", "**MME <= 20**", "**MME >= 50**", "**MME >= 90**",
                  "**Days <= 7**", "**Days > 7**", "**Days > 30**",
                  "**MME <= 20, Days <= 7**", "**MME >= 50, Days > 7**", "**MME >= 90, Days > 7**", "**MME >= 50, Days > 30**", "**MME >= 90, Days > 30**")
)

latex_dem_tbl <- dem_tbl |>
  as_gt() |>
  as_latex()

dem_tbl_df <- dem_tbl |>
  as_gt() |>
  as.data.frame() |>
  select(-starts_with("var_type_"))


## Comorbidities
comorb_tbl_cohort <- comorb_tbl_data |>
  filter(subset_cohort == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days#,
            #subset_cohort
  )) |>
  tbl_summary(missing = "ifany",
              include = -"subset_cohort") |>
  bold_labels() |>
  modify_header(label ~ "**Cohort**")

comorb_tbl_subset_B_under_20 <- comorb_tbl_data |>
  filter(subset_B_under_20 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            #subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_under_20) |>
  bold_labels() |>
  modify_header(label ~ "**MME <= 20**")

comorb_tbl_B1 <- comorb_tbl_data |>
  filter(subset_B1 == TRUE) |>
  select(-c(#subset_B1,
    subset_B2,
    subset_B3,
    subset_B4,
    subset_B5,
    subset_B6,
    subset_B7,
    subset_B8,
    subset_B_under_20,
    subset_B_days_7_dose_under_20,
    subset_B_not_risky_days,
    subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B1) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50**")

comorb_tbl_B4 <- comorb_tbl_data |>
  filter(subset_B4 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            #subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B4) |>
  bold_labels() |>
  modify_header(label ~ "**MME >= 90**")

comorb_tbl_subset_B_not_risky_days <- comorb_tbl_data |>
  filter(subset_B_not_risky_days == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            #subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_not_risky_days) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days <= 7**")

comorb_tbl_B2 <- comorb_tbl_data |>
  filter(subset_B2 == TRUE) |>
  select(-c(subset_B1,
            #subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B2) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days > 7**")

comorb_tbl_B6 <- comorb_tbl_data |>
  filter(subset_B6 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            #subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B6) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days > 30**")

comorb_tbl_subset_B_days_7_dose_under_20 <- comorb_tbl_data |>
  filter(subset_B_days_7_dose_under_20 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            #subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_days_7_dose_under_20) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME <= 50, Days <= 7**")

comorb_tbl_B3 <- comorb_tbl_data |>
  filter(subset_B3 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            #subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B3) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50, Days > 7**")

comorb_tbl_B5 <- comorb_tbl_data |>
  filter(subset_B5 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            #subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B5) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 90, Days > 7**")

comorb_tbl_B7 <- comorb_tbl_data |>
  filter(subset_B7 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            #subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B7) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50, Days > 30**")

comorb_tbl_B8 <- comorb_tbl_data |>
  filter(subset_B8 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            #subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B8) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 90, Days > 30**")

comorb_tbl <- tbl_merge(
  tbls = list(comorb_tbl_cohort, comorb_tbl_subset_B_under_20, comorb_tbl_B1, comorb_tbl_B4,
              comorb_tbl_subset_B_not_risky_days, comorb_tbl_B2, comorb_tbl_B6, 
              comorb_tbl_subset_B_days_7_dose_under_20, comorb_tbl_B3, comorb_tbl_B5, comorb_tbl_B7, comorb_tbl_B8),
  tab_spanner = c("**Cohort**", "**MME <= 20**", "**MME >= 50**", "**MME >= 90**",
                  "**Days <= 7**", "**Days > 7**", "**Days > 30**",
                  "**MME <= 20, Days <= 7**", "**MME >= 50, Days > 7**", "**MME >= 90, Days > 7**", "**MME >= 50, Days > 30**", "**MME >= 90, Days > 30**")
)

latex_comorb_tbl <- comorb_tbl |>
  as_gt() |>
  as_latex()

comorb_tbl_df <- comorb_tbl |>
  as_gt() |>
  as.data.frame() |>
  select(-starts_with("var_type_"))

## Exposure
exp_tbl_cohort <- exp_cond_tbl_data |>
  filter(subset_cohort == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days#,
            #subset_cohort
  )) |>
  tbl_summary(missing = "ifany",
              include = -"subset_cohort") |>
  bold_labels() |>
  modify_header(label ~ "**Cohort**")

exp_tbl_subset_B_under_20 <- exp_cond_tbl_data |>
  filter(subset_B_under_20 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            #subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_under_20) |>
  bold_labels() |>
  modify_header(label ~ "**MME <= 20**")

exp_tbl_B1 <- exp_cond_tbl_data |>
  filter(subset_B1 == TRUE) |>
  select(-c(#subset_B1,
    subset_B2,
    subset_B3,
    subset_B4,
    subset_B5,
    subset_B6,
    subset_B7,
    subset_B8,
    subset_B_under_20,
    subset_B_days_7_dose_under_20,
    subset_B_not_risky_days,
    subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B1) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50**")

exp_tbl_B4 <- exp_cond_tbl_data |>
  filter(subset_B4 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            #subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B4) |>
  bold_labels() |>
  modify_header(label ~ "**MME >= 90**")

exp_tbl_subset_B_not_risky_days <- exp_cond_tbl_data |>
  filter(subset_B_not_risky_days == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            #subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_not_risky_days,
              statistic = all_continuous() ~ "{median} ({p25}, {p75})",
              type = list(
                exposure_days_supply ~ "continuous")) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days <= 7**")

exp_tbl_B2 <- exp_cond_tbl_data |>
  filter(subset_B2 == TRUE) |>
  select(-c(subset_B1,
            #subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B2) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days > 7**")

exp_tbl_B6 <- exp_cond_tbl_data |>
  filter(subset_B6 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            #subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B6) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days > 30**")

exp_tbl_subset_B_days_7_dose_under_20 <- exp_cond_tbl_data |>
  filter(subset_B_days_7_dose_under_20 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            #subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_days_7_dose_under_20,
              statistic = all_continuous() ~ "{median} ({p25}, {p75})",
              type = list(
                exposure_days_supply ~ "continuous")) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME <= 50, Days <= 7**")

exp_tbl_B3 <- exp_cond_tbl_data |>
  filter(subset_B3 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            #subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B3) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50, Days > 7**")

exp_tbl_B5 <- exp_cond_tbl_data |>
  filter(subset_B5 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            #subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B5) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 90, Days > 7**")

exp_tbl_B7 <- exp_cond_tbl_data |>
  filter(subset_B7 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            #subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B7) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50, Days > 30**")

exp_tbl_B8 <- exp_cond_tbl_data |>
  filter(subset_B8 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            #subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B8) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 90, Days > 30**")

exp_tbl <- tbl_merge(
  tbls = list(exp_tbl_cohort, exp_tbl_subset_B_under_20, exp_tbl_B1, exp_tbl_B4,
              exp_tbl_subset_B_not_risky_days, exp_tbl_B2, exp_tbl_B6, 
              exp_tbl_subset_B_days_7_dose_under_20, exp_tbl_B3, exp_tbl_B5, exp_tbl_B7, exp_tbl_B8),
  tab_spanner = c("**Cohort**", "**MME <= 20**", "**MME >= 50**", "**MME >= 90**",
                  "**Days <= 7**", "**Days > 7**", "**Days > 30**",
                  "**MME <= 20, Days <= 7**", "**MME >= 50, Days > 7**", "**MME >= 90, Days > 7**", "**MME >= 50, Days > 30**", "**MME >= 90, Days > 30**")
)

latex_exp_tbl <- exp_tbl |>
  as_gt() |>
  as_latex()

exp_tbl_df <- exp_tbl |>
  as_gt() |>
  as.data.frame() |>
  select(-starts_with("var_type_"))

## OUD

oud_tbl_cohort <- oud_tbl_data |>
  filter(subset_cohort == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days#,
            #subset_cohort
  )) |>
  tbl_summary(missing = "ifany",
              include = -"subset_cohort") |>
  bold_labels() |>
  modify_header(label ~ "**Cohort**")

oud_tbl_subset_B_under_20 <- oud_tbl_data |>
  filter(subset_B_under_20 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            #subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_under_20) |>
  bold_labels() |>
  modify_header(label ~ "**MME <= 20**")

oud_tbl_B1 <- oud_tbl_data |>
  filter(subset_B1 == TRUE) |>
  select(-c(#subset_B1,
    subset_B2,
    subset_B3,
    subset_B4,
    subset_B5,
    subset_B6,
    subset_B7,
    subset_B8,
    subset_B_under_20,
    subset_B_days_7_dose_under_20,
    subset_B_not_risky_days,
    subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B1) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50**")

oud_tbl_B4 <- oud_tbl_data |>
  filter(subset_B4 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            #subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B4) |>
  bold_labels() |>
  modify_header(label ~ "**MME >= 90**")

oud_tbl_subset_B_not_risky_days <- oud_tbl_data |>
  filter(subset_B_not_risky_days == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            #subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_not_risky_days) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days <= 7**")

oud_tbl_B2 <- oud_tbl_data |>
  filter(subset_B2 == TRUE) |>
  select(-c(subset_B1,
            #subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B2) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days > 7**")

oud_tbl_B6 <- oud_tbl_data |>
  filter(subset_B6 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            #subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B6) |>
  bold_labels() |>
  modify_header(label ~ "**Subset Days > 30**")

oud_tbl_subset_B_days_7_dose_under_20 <- oud_tbl_data |>
  filter(subset_B_days_7_dose_under_20 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            #subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B_days_7_dose_under_20) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME <= 50, Days <= 7**")

oud_tbl_B3 <- oud_tbl_data |>
  filter(subset_B3 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            #subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B3) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50, Days > 7**")

oud_tbl_B5 <- oud_tbl_data |>
  filter(subset_B5 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            #subset_B5,
            subset_B6,
            subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B5) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 90, Days > 7**")

oud_tbl_B7 <- oud_tbl_data |>
  filter(subset_B7 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            #subset_B7,
            subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B7) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 50, Days > 30**")

oud_tbl_B8 <- oud_tbl_data |>
  filter(subset_B8 == TRUE) |>
  select(-c(subset_B1,
            subset_B2,
            subset_B3,
            subset_B4,
            subset_B5,
            subset_B6,
            subset_B7,
            #subset_B8,
            subset_B_under_20,
            subset_B_days_7_dose_under_20,
            subset_B_not_risky_days,
            subset_cohort)) |>
  tbl_summary(missing = "ifany",
              include = -subset_B8) |>
  bold_labels() |>
  modify_header(label ~ "**Subset MME >= 90, Days > 30**")

oud_tbl <- tbl_merge(
  tbls = list(oud_tbl_cohort, oud_tbl_subset_B_under_20, oud_tbl_B1, oud_tbl_B4,
              oud_tbl_subset_B_not_risky_days, oud_tbl_B2, oud_tbl_B6, 
              oud_tbl_subset_B_days_7_dose_under_20, oud_tbl_B3, oud_tbl_B5, oud_tbl_B7, oud_tbl_B8),
  tab_spanner = c("**Cohort**", "**MME <= 20**", "**MME >= 50**", "**MME >= 90**",
                  "**Days <= 7**", "**Days > 7**", "**Days > 30**",
                  "**MME <= 20, Days <= 7**", "**MME >= 50, Days > 7**", "**MME >= 90, Days > 7**", "**MME >= 50, Days > 30**", "**MME >= 90, Days > 30**")
)

latex_oud_tbl <- oud_tbl |>
  as_gt() |>
  as_latex()

oud_tbl_df <- oud_tbl |>
  as_gt() |>
  as.data.frame() |>
  select(-starts_with("var_type_"))


# Print out Latex code ---------------------------------------------------------

# Full cohort
cat(latex_dem_tbl)
cat(latex_comorb_tbl)
cat(latex_exp_tbl)
cat(latex_oud_tbl)
