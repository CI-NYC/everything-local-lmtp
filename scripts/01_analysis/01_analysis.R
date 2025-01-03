# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
#devtools::install_github("nt-williams/riesznet", auth_token = "TOKEN", force = TRUE)
library(lmtp, lib.loc = "/mnt/dev/lmtp-local-riesznet")
library(riesznet)
library(tidyverse)
library(mlr3superlearner)
library(mlr3extralearners)
library(glmnet)
library(earth)
library(lightgbm)
library(ranger)

drv_root <- "/mnt/general-data/disability/everything-local-lmtp/"

df <- fst::read_fst(paste0(drv_root ,"msk_cohort_clean_imputed.fst")) |>
  as.data.table() |>
  mutate(subset_B4 = ifelse(exposure_max_daily_dose_mme >= 90, TRUE, FALSE),
         subset_B5 = ifelse(exposure_max_daily_dose_mme >= 90 & exposure_days_supply > 7, TRUE, FALSE),
         subset_B6 = ifelse(exposure_days_supply > 30, TRUE, FALSE),
         subset_B7 = ifelse(exposure_days_supply > 30 & exposure_max_daily_dose_mme >= 50, TRUE, FALSE),
         subset_B8 = ifelse(exposure_days_supply > 30 & exposure_max_daily_dose_mme >= 90, TRUE, FALSE)
  ) |>
  mutate(subset_B_not_risky_days = ifelse(exposure_days_supply <= 7, TRUE, FALSE),
         subset_B_under_20 = ifelse(exposure_max_daily_dose_mme <= 20, TRUE, FALSE),
         subset_B_days_7_dose_under_20 = ifelse(exposure_days_supply <= 7 & exposure_max_daily_dose_mme <= 20, TRUE, FALSE),
         subset_cohort = TRUE)

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

# d1 <- function(data, a) {
#   out <- list(
#     data[[a[1]]]*0.8, 
#     data[[a[2]]]
#   )
#   setNames(out, a)
# }
# d2 <- function(data, a) {
#   out <- list(
#     data[[a[1]]], 
#     data[[a[2]]]*0.8
#   )
#   setNames(out, a)
# }
# d3 <- function(data, a) {
#   out <- list(
#     data[[a[1]]]*0.8, 
#     data[[a[2]]]*0.8
#   )
#   setNames(out, a)
# }


run_lmtp <- function(data, t, shift, conditional)
{
  data <- data |>
    as.data.frame()
  
  if(conditional == "cohort")
  {
    conditional_matrix <- NULL
  } else{
    conditional_matrix <- data |>
      select(conditional) |>
      mutate(across(all_of(conditional), ~ as.logical(.))) |>
      as.matrix()
  }
  
  if(conditional %in% c("cohort", "subset_B_not_risky_days", "subset_B_under_20", "subset_B_days_7_dose_under_20"))
  {
  if(shift == "obs")
  {
    shifted <- NULL
  } else if (shift == "d1")
  {
    shifted <- data |>
      mutate(exposure_max_daily_dose_mme = ifelse(0.8 * exposure_max_daily_dose_mme >= min(exposure_max_daily_dose_mme), 0.8 * exposure_max_daily_dose_mme, exposure_max_daily_dose_mme), # reduce MME by 20%
             cens_period_1 = 1,
             cens_period_2 = 1,
             cens_period_3 = 1,
             cens_period_4 = 1,
             cens_period_5 = 1)
  } else if (shift == "d2")
  {
    shifted <- data |>
      mutate(exposure_days_supply = ifelse(0.8 * exposure_days_supply >= min(exposure_days_supply), 0.8 * exposure_days_supply, exposure_days_supply), # reduce days supplied by 20%
             cens_period_1 = 1,
             cens_period_2 = 1,
             cens_period_3 = 1,
             cens_period_4 = 1,
             cens_period_5 = 1)
  }else 
  {
    shifted <- data |>
      mutate(exposure_max_daily_dose_mme = ifelse(0.8 * exposure_max_daily_dose_mme >= min(exposure_max_daily_dose_mme), 0.8 * exposure_max_daily_dose_mme, exposure_max_daily_dose_mme), # reduce MME by 20%
             exposure_days_supply = ifelse(0.8 * exposure_days_supply >= min(exposure_days_supply), 0.8 * exposure_days_supply, exposure_days_supply), # reduce days supplied by 20%
             cens_period_1 = 1,
             cens_period_2 = 1,
             cens_period_3 = 1,
             cens_period_4 = 1,
             cens_period_5 = 1)
  } 
  } else
  {
    if(shift == "obs")
    {
      shifted <- NULL
    } else if (shift == "d1")
    {
      shifted <- data |>
        mutate(exposure_max_daily_dose_mme = 0.8 * exposure_max_daily_dose_mme, # reduce MME by 20%
               cens_period_1 = 1,
               cens_period_2 = 1,
               cens_period_3 = 1,
               cens_period_4 = 1,
               cens_period_5 = 1)
    } else if (shift == "d2")
    {
      shifted <- data |>
        mutate(exposure_days_supply =  0.8 * exposure_days_supply, # reduce days supplied by 20%
               cens_period_1 = 1,
               cens_period_2 = 1,
               cens_period_3 = 1,
               cens_period_4 = 1,
               cens_period_5 = 1)
    }else 
    {
      shifted <- data |>
        mutate(exposure_max_daily_dose_mme = 0.8 * exposure_max_daily_dose_mme, # reduce MME by 20%
               exposure_days_supply = 0.8 * exposure_days_supply, # reduce days supplied by 20%
               cens_period_1 = 1,
               cens_period_2 = 1,
               cens_period_3 = 1,
               cens_period_4 = 1,
               cens_period_5 = 1)
    }
    
  }
  
  est <- progressr::with_progress(lmtp_tmle(data = data,
                                            trt = list(c("exposure_max_daily_dose_mme", "exposure_days_supply")),
                                            outcome = paste0("oud_period_", 1:t),
                                            baseline = W, 
                                            cens = paste0("cens_period_", 1:t),
                                            shifted = shifted,
                                            conditional = conditional_matrix, 
                                            riesz = TRUE, # must be true if conditional supplied
                                            mtp = TRUE,
                                            outcome_type = ifelse(t == 1, "binomial", "survival"),
                                            learners_outcome = c("mean", "glm", "earth", "lightgbm"),
                                            learners_trt = c("mean", "glm", "earth", "lightgbm"),
                                            folds = 2,
                                            control = lmtp_control(.learners_outcome_folds = 2,
                                                                   .learners_trt_folds = 2,
                                                                   .learners_conditional_folds = 2,
                                                                   .patience = 10,
                                                                   .epochs = 50L,
                                                                   .batch_size = 256,
                                                                   .learning_rate = 0.1,
                                                                   .weight_decay = 1
                                            ))
  )
  
  est
}

set.seed(5)
for(t in 3:5)
{
  for(shift in c(#"obs"#,
                "d1"#,
                 #"d2"#,
                 #"d3"#,
  ))
  { 
    for(subset in c(#"subset_B1"#, 
      #"subset_B2"#,
      #"subset_B3"#,
      #"subset_B4"#,
      #"subset_B5"#,
      #"subset_B6"#,
      #"subset_B7"#,
      #"subset_B8"#,
      #"cohort"#,
      #"subset_B_not_risky_days"#,
      "subset_B_under_20"#,
      #"subset_B_days_7_dose_under_20"#,
    ))
    {
      finished <- FALSE
      
      while(!finished){ # if failed on previous iteration, try again
        
        set.seed(5)
        tryCatch({
          results <- run_lmtp(df, t, shift, subset)
          
          finished <- TRUE
          
          saveRDS(results, paste0("/mnt/general-data/disability/everything-local-lmtp/results_final/", shift, "_", subset, "_time_", t, ".rds"))
        }, error = function(e){
          cat("Error on iteration ", t, "shift: ", shift, "subset: ", subset,
              e$message)})
      }
      print(t)
    }
  }
}
