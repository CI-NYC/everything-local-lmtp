# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
library(knitr)

drv_root <- "/mnt/general-data/disability/everything-local-lmtp/"

df <- fst::read_fst(paste0(drv_root ,"msk_cohort_clean_imputed.fst")) |>
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

# empty dataframe to store results
results <- data.frame(
  Subgroup = c("Cohort", "Subset B0A", "Subset B0B", "Subset B0C", 
               "Subset B1", "Subset B4", "Subset B2", "Subset B6", 
               "Subset B3", "Subset B5", "Subset B7", "Subset B8"),
  MME = NA,
  Days = NA
)

results[1, 2] <- df |> 
  mutate(min_dose = min(exposure_max_daily_dose_mme)) |>
  mutate(exposure_max_daily_dose_mme_new = ifelse(0.8 * exposure_max_daily_dose_mme >= min_dose, 0.8 * exposure_max_daily_dose_mme, exposure_max_daily_dose_mme)) |> 
  mutate(diff = exposure_max_daily_dose_mme - exposure_max_daily_dose_mme_new) |> 
  summarize(round(mean(diff), 2))

results[1, 3] <- df |> 
  mutate(min_days = min(exposure_days_supply)) |>
  mutate(exposure_days_supply_new = ifelse(0.8 * exposure_days_supply >= min_days, 0.8 * exposure_days_supply, exposure_days_supply)) |> 
  mutate(diff = exposure_days_supply - exposure_days_supply_new) |> 
  summarize(round(mean(diff), 2))

results[2, 2] <- df |> 
  mutate(min_dose = min(exposure_max_daily_dose_mme)) |>
  filter(subset_B_under_20 == TRUE) |>
  mutate(exposure_max_daily_dose_mme_new = ifelse(0.8 * exposure_max_daily_dose_mme >= min_dose, 0.8 * exposure_max_daily_dose_mme, exposure_max_daily_dose_mme)) |> 
  mutate(exposure_max_daily_dose_mme_new = 0.8 * exposure_max_daily_dose_mme) |> 
  mutate(diff = exposure_max_daily_dose_mme - exposure_max_daily_dose_mme_new) |> 
  summarize(round(mean(diff), 2))

results[3, 3] <- df |> 
  mutate(min_days = min(exposure_days_supply)) |>
  filter(subset_B_not_risky_days == TRUE) |>
  mutate(exposure_days_supply_new = ifelse(0.8 * exposure_days_supply >= min_days, 0.8 * exposure_days_supply, exposure_days_supply)) |> 
  mutate(diff = exposure_days_supply - exposure_days_supply_new) |> 
  summarize(round(mean(diff), 2))

results[4, 2] <- df |> 
  mutate(min_dose = min(exposure_max_daily_dose_mme)) |>
  filter(subset_B_days_7_dose_under_20 == TRUE) |>
  mutate(exposure_max_daily_dose_mme_new = ifelse(0.8 * exposure_max_daily_dose_mme >= min_dose, 0.8 * exposure_max_daily_dose_mme, exposure_max_daily_dose_mme)) |> 
  mutate(exposure_max_daily_dose_mme_new = 0.8 * exposure_max_daily_dose_mme) |> 
  mutate(diff = exposure_max_daily_dose_mme - exposure_max_daily_dose_mme_new) |> 
  summarize(round(mean(diff), 2))

results[4, 3] <- df |> 
  mutate(min_days = min(exposure_days_supply)) |>
  filter(subset_B_days_7_dose_under_20 == TRUE) |>
  mutate(exposure_days_supply_new = ifelse(0.8 * exposure_days_supply >= min_days, 0.8 * exposure_days_supply, exposure_days_supply)) |> 
  mutate(diff = exposure_days_supply - exposure_days_supply_new) |> 
  summarize(round(mean(diff), 2))

results[5, 2] <- df |> 
  filter(subset_B1 == TRUE) |> 
  mutate(exposure_max_daily_dose_mme_new = 0.8 * exposure_max_daily_dose_mme) |> 
  mutate(diff = exposure_max_daily_dose_mme - exposure_max_daily_dose_mme_new) |> 
  summarize(round(mean(diff), 2))

results[6, 2] <- df |> 
  filter(subset_B4 == TRUE) |> 
  mutate(exposure_max_daily_dose_mme_new = 0.8 * exposure_max_daily_dose_mme) |> 
  mutate(diff = exposure_max_daily_dose_mme - exposure_max_daily_dose_mme_new) |> 
  summarize(round(mean(diff), 2))

results[7, 3] <- df |> 
  filter(subset_B2 == TRUE) |>
  mutate(exposure_days_supply_new = 0.8 * exposure_days_supply) |> 
  mutate(diff = exposure_days_supply - exposure_days_supply_new) |> 
  summarize(round(mean(diff), 2))

results[8, 3] <- df |> 
  filter(subset_B6 == TRUE) |>
  mutate(exposure_days_supply_new = 0.8 * exposure_days_supply) |> 
  mutate(diff = exposure_days_supply - exposure_days_supply_new) |> 
  summarize(round(mean(diff), 2))

results[9, 2] <- df |> 
  filter(subset_B3 == TRUE) |> 
  mutate(exposure_max_daily_dose_mme_new = 0.8 * exposure_max_daily_dose_mme) |> 
  mutate(diff = exposure_max_daily_dose_mme - exposure_max_daily_dose_mme_new) |> 
  summarize(round(mean(diff), 2))

results[9, 3] <- df |> 
  filter(subset_B3 == TRUE) |>
  mutate(exposure_days_supply_new = 0.8 * exposure_days_supply) |> 
  mutate(diff = exposure_days_supply - exposure_days_supply_new) |> 
  summarize(round(mean(diff), 2))

results[10, 2] <- df |> 
  filter(subset_B5 == TRUE) |> 
  mutate(exposure_max_daily_dose_mme_new = 0.8 * exposure_max_daily_dose_mme) |> 
  mutate(diff = exposure_max_daily_dose_mme - exposure_max_daily_dose_mme_new) |> 
  summarize(round(mean(diff), 2))

results[10, 3] <- df |> 
  filter(subset_B5 == TRUE) |>
  mutate(exposure_days_supply_new = 0.8 * exposure_days_supply) |> 
  mutate(diff = exposure_days_supply - exposure_days_supply_new) |> 
  summarize(round(mean(diff), 2))

results[11, 2] <- df |> 
  filter(subset_B7 == TRUE) |> 
  mutate(exposure_max_daily_dose_mme_new = 0.8 * exposure_max_daily_dose_mme) |> 
  mutate(diff = exposure_max_daily_dose_mme - exposure_max_daily_dose_mme_new) |> 
  summarize(round(mean(diff), 2))

results[11, 3] <- df |> 
  filter(subset_B7 == TRUE) |>
  mutate(exposure_days_supply_new = 0.8 * exposure_days_supply) |> 
  mutate(diff = exposure_days_supply - exposure_days_supply_new) |> 
  summarize(round(mean(diff), 2))

results[12, 2] <- df |> 
  filter(subset_B8 == TRUE) |> 
  mutate(exposure_max_daily_dose_mme_new = 0.8 * exposure_max_daily_dose_mme) |> 
  mutate(diff = exposure_max_daily_dose_mme - exposure_max_daily_dose_mme_new) |> 
  summarize(round(mean(diff), 2))

results[12, 3] <- df |> 
  filter(subset_B8 == TRUE) |>
  mutate(exposure_days_supply_new = 0.8 * exposure_days_supply) |> 
  mutate(diff = exposure_days_supply - exposure_days_supply_new) |> 
  summarize(round(mean(diff), 2))

kable(results, format = "latex", caption = "Shifts")









