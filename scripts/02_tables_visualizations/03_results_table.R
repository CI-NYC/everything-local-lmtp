# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
library(knitr)
library(kableExtra)

combined_results_list <- readRDS(here::here("results/primary_combined_results_list.rds"))
contrast_results_list <- readRDS(here::here("results/primary_contrast_results_list.rds"))

combined_results_list <- lapply(combined_results_list, function(x) {
  x |> 
    mutate(estimate = paste0(round(estimate, 4), " (", round(conf.low, 4), ", ", round(conf.high, 4), ")")) |>
    select(-c(estimator, std.error, conf.low, conf.high)) |>
    pivot_wider(names_from = t, 
                values_from = estimate,
                names_glue = "estimate_{t}")
})

combined_results_list <- lapply(names(combined_results_list), function(name) {
  df <- combined_results_list[[name]]
  df$subgroup <- name  
  return(df)
})

combined_df <- bind_rows(combined_results_list) |>
  relocate(subgroup, .before = shift) |>
  mutate(subgroup = case_when(subgroup == "cohort" & shift == "d1" ~ "cohort_d1",
                              subgroup == "cohort" & shift == "d2" ~ "cohort_d2",
                              subgroup == "cohort" & shift == "d3" ~ "cohort_d3",
                              TRUE ~ subgroup))

contrast_results_list <- lapply(contrast_results_list, function(x) {
  x |> 
    mutate(theta = paste0(round(theta, 4), " (", round(conf.low, 4), ", ", round(conf.high, 4), ")")) |>
    select(-c(std.error, conf.low, conf.high, shift, ref, p.value)) |>
    pivot_wider(names_from = t, 
                values_from = theta,
                names_glue = "theta_{t}")
})

contrast_results_list <- lapply(names(contrast_results_list), function(name) {
  df <- contrast_results_list[[name]]
  df$subgroup <- name  
  return(df)
})

contrast_df <- bind_rows(contrast_results_list) |>
  relocate(subgroup, .before = theta_1) |>
  mutate(subgroup = case_when(contrast == "d1 v. obs" ~ "cohort_d1",
                              contrast == "d2 v. obs" ~ "cohort_d2",
                              contrast == "d3 v. obs" ~ "cohort_d3",
                              TRUE ~ subgroup)) |>
  select(-contrast)

joined_df <- combined_df |>
  left_join(contrast_df) |>
  mutate(subgroup = factor(subgroup, levels = c("cohort",
                                                "cohort_d1",
                                                "cohort_d2",
                                                "cohort_d3",
                                                "subset_B_under_20",
                                                "subset_B1",
                                                "subset_B4",
                                                "subset_B_not_risky_days",
                                                "subset_B2",
                                                "subset_B6",
                                                "subset_B_days_7_dose_under_20",
                                                "subset_B3",
                                                "subset_B5",
                                                "subset_B7",
                                                "subset_B8"
                                                ))) |>
  arrange(subgroup)

joined_df <- joined_df |> select(subgroup,
                    shift,
                    estimate_1,
                    theta_1,
                    estimate_2,
                    theta_2,
                    estimate_3,
                    theta_3,
                    estimate_4,
                    theta_4,
                    estimate_5,
                    theta_5) |>
  mutate(theta_1 = ifelse(shift == "obs", NA, theta_1),
         theta_2 = ifelse(shift == "obs", NA, theta_2),
         theta_3 = ifelse(shift == "obs", NA, theta_3),
         theta_4 = ifelse(shift == "obs", NA, theta_4),
         theta_5 = ifelse(shift == "obs", NA, theta_5)
         )

kable(joined_df, format = "latex")






