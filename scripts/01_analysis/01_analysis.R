# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
#devtools::install_github("shodaiinose/lmtp", ref = "riesz-local")
library(lmtplocal)
library(tidyverse)


df <- readRDS("~/df.rds")

W <- c(
  "dem_age",
  # "dem_sex",
  "dem_sex_m",
  # "dem_race",
  "dem_race_aian",
  "dem_race_asian",
  "dem_race_black",
  "dem_race_hawaiian",
  "dem_race_hispanic",
  "dem_race_multiracial",
  "dem_primary_language_english", # NAs
  "dem_married_or_partnered", # NAs
  # "dem_household_size",
  "dem_household_size_2",
  "dem_household_size_2plus",
  "dem_veteran", # NAs
  "dem_probable_high_income",
  "dem_tanf_benefits", # NAs
  # "dem_ssi_benefits", # character
  "dem_ssi_benefits_mandatory_optional",
  "bipolar_washout_cal",
  "anxiety_washout_cal",
  "adhd_washout_cal",
  "depression_washout_cal",
  "mental_ill_washout_cal",
  # NA/missing indicators
  "missing_dem_race",
  "missing_dem_primary_language_english",
  "missing_dem_married_or_partnered",
  "missing_dem_household_size",
  "missing_dem_veteran",
  "missing_dem_tanf_benefits",
  "missing_dem_ssi_benefits"
) 

## Shift functions

d1 <- function(data, a) {
  out <- list(
    data[[a[1]]]*0.8, 
    data[[a[2]]]
  )
  setNames(out, a)
}
d2 <- function(data, a) {
  out <- list(
    data[[a[1]]], 
    data[[a[2]]]*0.8
  )
  setNames(out, a)
}
d3 <- function(data, a) {
  out <- list(
    data[[a[1]]]*0.8, 
    data[[a[2]]]*0.8
  )
  setNames(out, a)
}


run_lmtp <- function(data, shift, conditional)
{
  data <- data |>
    as.data.frame()
  
  conditional_matrix <- data |>
    select(conditional) |>
    mutate(across(all_of(conditional), ~ as.logical(.))) |>
    as.matrix()
  
  if(shift == "obs")
  {
    shift_function <- NULL
  } else if (shift == "d1")
  {
    shift_function <- d1
  } else if (shift == "d2")
  {
    shift_function <- d2
  }else 
  {
    shift_function <- d3
  } 
  
  est <- lmtp_survival(data = data,
                       trt = list(c("exposure_max_daily_dose_mme", "exposure_days_supply")),
                       outcome = paste0("oud_period_", 1:5),
                       baseline = W, 
                       cens = paste0("cens_period_", 1:5),
                       shift = shift_function,
                       conditional = conditional_matrix, # n x Tau, logical matrix
                       riesz = TRUE, # must be true if conditional supplied
                       mtp = TRUE,
                       estimator = c("lmtp_tmle"),
                       learners_outcome = c("mean", "glm", "glmnet", "ranger", "earth", "xgboost"),
                       learners_trt = c("mean", "glm", "glmnet", "ranger", "earth", "xgboost"),
                       folds = 2,
                       control = lmtp_control(.learners_outcome_folds = 2,
                                              .learners_trt_folds = 2)
  )

est
}

for(shift in c("obs",
               "d1",
              "d2",
               "d3"
              ))
{
for(subset in c("subset_B1", 
                "subset_B2",
                "subset_B3"
                ))
{
  results <- run_lmtp(df, shift, subset)

  saveRDS(results, paste0("~/results/local_lmtp_results_", shift, "_", subset))
}
}




