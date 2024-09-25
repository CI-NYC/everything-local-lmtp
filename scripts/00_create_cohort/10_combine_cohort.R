# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
cohort <- load_data("msk_cohort.fst")

adhd <- readRDS("~/comorbidity/adhd.rds")
anxiety <- readRDS("~/comorbidity/anxiety.rds")
bipolar <- readRDS("~/comorbidity/bipolar.rds")
depression <- readRDS("~/comorbidity/depression.rds")
mental_ill <- readRDS("~/comorbidity/mental_ill.rds")

cohort_MH_joined <- cohort |>
  left_join(adhd |> select(BENE_ID, adhd_washout_6mos_cal) |> rename("adhd_washout_cal" = "adhd_washout_6mos_cal")) |>
  left_join(anxiety |> select(BENE_ID, anxiety_washout_6mos_cal) |> rename("anxiety_washout_cal" = "anxiety_washout_6mos_cal")) |>
  left_join(bipolar |> select(BENE_ID, bipolar_washout_6mos_cal) |> rename("bipolar_washout_cal" = "bipolar_washout_6mos_cal")) |>
  left_join(depression |> select(BENE_ID, depression_washout_6mos_cal) |> rename("depression_washout_cal" = "depression_washout_6mos_cal")) |>
  left_join(mental_ill |> select(BENE_ID, mental_ill_washout_6mos_cal) |> rename("mental_ill_washout_cal" = "mental_ill_washout_6mos_cal")) |>
  select(BENE_ID, 
         ends_with("dt", ignore.case = FALSE), 
         starts_with("dem"),
         ends_with("_washout_cal"),
         starts_with("exposure"), 
         starts_with("subset"), 
         cens_period_1, oud_period_1, 
         cens_period_2, oud_period_2, 
         cens_period_3, oud_period_3, 
         cens_period_4, oud_period_4, 
         cens_period_5, oud_period_5)

write_data(cohort, "msk_cohort_with_MH.fst")
