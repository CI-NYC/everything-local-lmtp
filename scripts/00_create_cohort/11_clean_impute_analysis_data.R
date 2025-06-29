# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
#devtools::install_github("shodaiinose/lmtp", ref = "riesz-local")
library(tidyverse)
library(data.table)
library(stringr)
library(readr)

drv_root <- "/mnt/general-data/disability/everything-local-lmtp/"

df <- fst::read_fst(paste0(drv_root ,"msk_cohort_with_MH.fst")) |>
  as.data.table()


# Check for NAs in all variables before addressing
na_check <- sapply(df, \(x) any(is.na(x)))

# Create indicator variables for missing dem variables
df[, `:=`(missing_dem_race = fifelse(is.na(dem_race), 1, 0), 
          missing_dem_primary_language_english = fifelse(is.na(dem_primary_language_english), 1, 0),
          missing_dem_married_or_partnered = fifelse(is.na(dem_married_or_partnered), 1, 0),
          missing_dem_household_size = fifelse(is.na(dem_household_size), 1, 0),
          missing_dem_veteran = fifelse(is.na(dem_veteran), 1, 0),
          missing_dem_tanf_benefits = fifelse(is.na(dem_tanf_benefits), 1, 0),
          missing_dem_ssi_benefits = fifelse(is.na(dem_ssi_benefits), 1, 0))]

Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Set NA values to the mode
df[, `:=`(
  dem_race = fifelse(is.na(dem_race), 
                     Mode(dem_race), dem_race),
  dem_primary_language_english = fifelse(is.na(dem_primary_language_english), 
                                         Mode(dem_primary_language_english), 
                                         dem_primary_language_english),
  dem_married_or_partnered = fifelse(is.na(dem_married_or_partnered), 
                                     Mode(dem_married_or_partnered), 
                                     dem_married_or_partnered),
  dem_household_size = fifelse(is.na(dem_household_size), 
                               Mode(dem_household_size), 
                               dem_household_size),
  dem_veteran = fifelse(is.na(dem_veteran), 
                        Mode(dem_veteran), dem_veteran),
  dem_tanf_benefits = fifelse(is.na(dem_tanf_benefits), 
                              Mode(dem_tanf_benefits), dem_tanf_benefits),
  dem_ssi_benefits = fifelse(is.na(dem_ssi_benefits), 
                             Mode(dem_ssi_benefits), dem_ssi_benefits)
)]

# Check for NAs in all variables after addressing
na_check <- sapply(df, \(x) any(is.na(x)))
any(na_check)

# Dummy coding ------------------------------------------------------------

# Convert all non-numeric variables to numeric using dummy variable coding
non_numeric_cols <- sapply(df, \(x) !is.numeric(x))
non_numeric_cols <- names(which(non_numeric_cols))

# Convert categorical variables to character type
df[, (non_numeric_cols) := lapply(.SD, as.character), .SDcols = non_numeric_cols]

df[, `:=`(dem_sex_m = fifelse(dem_sex == "M", 1, 0), # Sex (reference category set to female)
          # Race (reference category set to white, non-Hispanic)
          dem_race_aian = fifelse(
            dem_race == "American Indian and Alaska Native (AIAN), non-Hispanic", 
            1, 0), 
          dem_race_asian = fifelse(dem_race == "Asian, non-Hispanic", 1, 0), 
          dem_race_black = fifelse(dem_race == "Black, non-Hispanic", 1, 0),
          dem_race_hawaiian = fifelse(dem_race == "Hawaiian/Pacific Islander", 1, 0),
          dem_race_hispanic = fifelse(dem_race == "Hispanic, all races", 1, 0),
          dem_race_multiracial = fifelse(dem_race == "Multiracial, non-Hispanic", 1, 0),
          # Household size (reference category set to 1)
          dem_household_size_2 = fifelse(dem_household_size == "2", 1, 0),
          dem_household_size_2plus = fifelse(dem_household_size == "2+", 1, 0),
          # SSI benefits (reference category set to not applicable)
          dem_ssi_benefits_mandatory_optional = fifelse(
            dem_ssi_benefits == "Mandatory or optional", 1, 0))]

# percent above 200 MME
df |>
  group_by(exposure_max_daily_dose_mme > 200) |>
  summarize(percent = n()/nrow(df) * 100)

# truncating max MME exposure due to low coverage (99th percentile as discussed)
df[, `:=`(exposure_max_daily_dose_mme = pmin(exposure_max_daily_dose_mme, 200))]

# Save in the mediation folder
fst::write_fst(df, file.path(drv_root, "msk_cohort_clean_imputed.fst"))


### 7 day gap

rm(df)

df <- fst::read_fst(paste0(drv_root ,"msk_cohort_with_MH_7_day_gap.fst")) |>
  as.data.table()


# Check for NAs in all variables before addressing
na_check <- sapply(df, \(x) any(is.na(x)))

# Create indicator variables for missing dem variables
df[, `:=`(missing_dem_race = fifelse(is.na(dem_race), 1, 0), 
          missing_dem_primary_language_english = fifelse(is.na(dem_primary_language_english), 1, 0),
          missing_dem_married_or_partnered = fifelse(is.na(dem_married_or_partnered), 1, 0),
          missing_dem_household_size = fifelse(is.na(dem_household_size), 1, 0),
          missing_dem_veteran = fifelse(is.na(dem_veteran), 1, 0),
          missing_dem_tanf_benefits = fifelse(is.na(dem_tanf_benefits), 1, 0),
          missing_dem_ssi_benefits = fifelse(is.na(dem_ssi_benefits), 1, 0))]

Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Set NA values to the mode
df[, `:=`(
  dem_race = fifelse(is.na(dem_race), 
                     Mode(dem_race), dem_race),
  dem_primary_language_english = fifelse(is.na(dem_primary_language_english), 
                                         Mode(dem_primary_language_english), 
                                         dem_primary_language_english),
  dem_married_or_partnered = fifelse(is.na(dem_married_or_partnered), 
                                     Mode(dem_married_or_partnered), 
                                     dem_married_or_partnered),
  dem_household_size = fifelse(is.na(dem_household_size), 
                               Mode(dem_household_size), 
                               dem_household_size),
  dem_veteran = fifelse(is.na(dem_veteran), 
                        Mode(dem_veteran), dem_veteran),
  dem_tanf_benefits = fifelse(is.na(dem_tanf_benefits), 
                              Mode(dem_tanf_benefits), dem_tanf_benefits),
  dem_ssi_benefits = fifelse(is.na(dem_ssi_benefits), 
                             Mode(dem_ssi_benefits), dem_ssi_benefits)
)]

# Check for NAs in all variables after addressing
na_check <- sapply(df, \(x) any(is.na(x)))
any(na_check)

# Dummy coding ------------------------------------------------------------

# Convert all non-numeric variables to numeric using dummy variable coding
non_numeric_cols <- sapply(df, \(x) !is.numeric(x))
non_numeric_cols <- names(which(non_numeric_cols))

# Convert categorical variables to character type
df[, (non_numeric_cols) := lapply(.SD, as.character), .SDcols = non_numeric_cols]

df[, `:=`(dem_sex_m = fifelse(dem_sex == "M", 1, 0), # Sex (reference category set to female)
          # Race (reference category set to white, non-Hispanic)
          dem_race_aian = fifelse(
            dem_race == "American Indian and Alaska Native (AIAN), non-Hispanic", 
            1, 0), 
          dem_race_asian = fifelse(dem_race == "Asian, non-Hispanic", 1, 0), 
          dem_race_black = fifelse(dem_race == "Black, non-Hispanic", 1, 0),
          dem_race_hawaiian = fifelse(dem_race == "Hawaiian/Pacific Islander", 1, 0),
          dem_race_hispanic = fifelse(dem_race == "Hispanic, all races", 1, 0),
          dem_race_multiracial = fifelse(dem_race == "Multiracial, non-Hispanic", 1, 0),
          # Household size (reference category set to 1)
          dem_household_size_2 = fifelse(dem_household_size == "2", 1, 0),
          dem_household_size_2plus = fifelse(dem_household_size == "2+", 1, 0),
          # SSI benefits (reference category set to not applicable)
          dem_ssi_benefits_mandatory_optional = fifelse(
            dem_ssi_benefits == "Mandatory or optional", 1, 0))]

# percent above 200 MME
df |>
  group_by(exposure_max_daily_dose_mme > 200) |>
  summarize(percent = n()/nrow(df) * 100)

# truncating max MME exposure due to low coverage (200 MME)
df[, `:=`(exposure_max_daily_dose_mme = pmin(exposure_max_daily_dose_mme, 200))]

# Save in the mediation folder
fst::write_fst(df, file.path(drv_root, "msk_cohort_clean_imputed_7_day_gap.fst"))