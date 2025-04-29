# -------------------------------------
# Script: 00_censoring.R
# Author: Nick Williams
# Purpose: Create censoring indicators
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)
library(lubridate)
library(arrow)
library(yaml)

source("R/helpers.R")

for (i in c("", "_7_day_gap"))
{

    cohort <- load_data(paste0("msk_washout_continuous_enrollment_opioid_requirements_with_exposures", i, ".fst"))
  
  # codes for dual eligibility
codes <- read_yaml("data/public/eligibility_codes.yml")

# Create outcome periods
cohort <- fmutate(
  cohort, 
  period_1 = interval(
    min_opioid_date, exposure_period_end_dt + days(91) # exposure period + period 1
  ), 
  period_2 = interval(
    int_end(period_1) + days(1), int_end(period_1) + days(91)
  ), 
  period_3 = interval(
    int_end(period_2) + days(1), int_end(period_2) + days(91)
  ), 
  period_4 = interval(
    int_end(period_3) + days(1), int_end(period_3) + days(91)
  ), 
  period_5 = interval(
    int_end(period_4) + days(1), int_end(period_4) + days(91)
  )
)

# Load demographics dataset
demo <- open_demo()

demo <- 
  filter(demo, BENE_ID %in% cohort$BENE_ID) |> 
  collect()

in_period <- function(data, date_col, period_col, prefix) {
  mutate(data, "{prefix}_{{ period_col }}" := as.numeric({{ date_col }} %within% {{ period_col }}))
}

add_all_periods <- function(data, date_col, prefix) {
    in_period(data, {{ date_col }}, period_1, prefix) |> 
    in_period({{ date_col }}, period_2, prefix) |> 
    in_period({{ date_col }}, period_3, prefix) |> 
    in_period({{ date_col }}, period_4, prefix) |> 
    in_period({{ date_col }}, period_5, prefix)
}

# age censoring -----------------------------------------------------------

age_cens <- 
  fselect(demo, BENE_ID, BIRTH_DT) |> 
  funique() |> 
  drop_na() |> 
  fmutate(bday_65 = BIRTH_DT + years(65)) |> 
  inner_join(cohort) |> 
  group_by(BENE_ID) |> 
  add_tally() |> 
  ungroup() |> 
  filter(n == 1) |> 
  add_all_periods(bday_65, "age_cens") |> 
  select(BENE_ID, starts_with("age_cens")) |> 
  right_join(cohort) |> 
  mutate(across(starts_with("age_cens"), replace_na)) |> 
  select(BENE_ID, starts_with("age_cens"))

# date censoring ----------------------------------------------------------

dec_cens <- 
  fmutate(cohort, cens_date = as.Date("2020-01-01")) |> 
  add_all_periods(cens_date, "dec_cens") |> 
  select(BENE_ID, starts_with("dec_cens"))

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

dual_cens <- 
  inner_join(cohort, dual_codes) |> 
  filter(elig_dt %within% interval(min_opioid_date, exposure_period_end_dt + days(455))) |> 
  group_by(BENE_ID) |>
  arrange(elig_dt) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  add_all_periods(elig_dt, "dual_elig_cens") |> 
  select(BENE_ID, starts_with("dual_elig_cens")) |> 
  right_join(cohort) |> 
  mutate(across(starts_with("dual_elig_cens"), replace_na)) |> 
  select(BENE_ID, starts_with("dual_elig_cens"))

dual_codes2 <- 
  select(demo, BENE_ID, RFRNC_YR, starts_with("ELGBLTY_GRP_CD"), -ELGBLTY_GRP_CD_LTST) |>
  pivot(ids = c("BENE_ID", "RFRNC_YR"), 
        how = "l", 
        names = list("month", "code")) |> 
  drop_na() |> 
  fmutate(month = str_extract(month, "\\d+$"), 
          year = as.numeric(RFRNC_YR),
          elig_dt = as.Date(paste0(year, "-", month, "-01"))) |> 
  join(cohort, how = "inner") |> 
  fselect(BENE_ID, washout_start_dt, min_opioid_date, code, elig_dt) |> 
  fsubset(code %in% codes$dual_eligibility)

dual_cens2 <- 
  inner_join(cohort, dual_codes2) |> 
  filter(elig_dt %within% interval(min_opioid_date, exposure_period_end_dt + days(455))) |> 
  group_by(BENE_ID) |>
  arrange(elig_dt) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  add_all_periods(elig_dt, "dual_elig2_cens") |> 
  select(BENE_ID, starts_with("dual_elig2_cens")) |> 
  right_join(cohort) |> 
  mutate(across(starts_with("dual_elig2_cens"), replace_na)) |> 
  select(BENE_ID, starts_with("dual_elig2_cens"))

enrollment_cens <- load_data(paste0("all_possible_enrollment_dates/censoring_enrollment", i, ".fst")) |>
  filter(BENE_ID %in% c(cohort$BENE_ID))

cens <- 
  list(
    age_cens,
    enrollment_cens,
    dec_cens, 
    dual_cens, 
    dual_cens2
  ) |> 
  reduce(left_join) |> 
  mutate(cens_period_1 = if_any(.cols = ends_with("period_1"), \(x) x == 1), 
         cens_period_2 = if_any(.cols = ends_with("period_2"), \(x) x == 1), 
         cens_period_3 = if_any(.cols = ends_with("period_3"), \(x) x == 1), 
         cens_period_4 = if_any(.cols = ends_with("period_4"), \(x) x == 1), 
         cens_period_5 = if_any(.cols = ends_with("period_5"), \(x) x == 1), 
         across(starts_with("cens_period"), as.numeric)) |> 
  select(BENE_ID, starts_with("cens_period")) |> 
  lmtp::event_locf(paste0("cens_period_", 1:5))

# flip censoring indicators (in lmtp, 1 indicates still observed)
cens <- mutate(cens, across(starts_with("cens"), \(x) ifelse(x == 0, 1, 0)))

write_data(cens, paste0("msk_washout_continuous_enrollment_opioid_requirements_censoring", i, ".fst"))
}
