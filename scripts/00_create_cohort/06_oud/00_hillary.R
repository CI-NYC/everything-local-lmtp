# -------------------------------------
# Script: 00_hillary.R
# Author: Nick Williams
# Purpose: Identify OUD using Hillary codes
# Notes:
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(fst)
library(yaml)

source("R/helpers.R")

# Source ICD codes
codes <- read_yaml("data/public/oud_codes.yml")$hillary

for (i in c("", "_7_day_gap"))
{

# load cohort  
cohort <- load_data(paste0("msk_washout_continuous_enrollment_opioid_requirements_with_exposures", i, ".fst"))

# Read in IPH dataset
iph <- open_iph()

# Read in OTH 
oth <- open_oth()

iph_hillary <-
  select(iph, BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
  collect() |> 
  inner_join(cohort) |> 
  mutate(SRVC_BGN_DT = case_when(
    is.na(SRVC_BGN_DT) ~ SRVC_END_DT, 
    TRUE ~ SRVC_BGN_DT)
  ) |> 
  pivot_longer(cols = contains("DGNS_CD"), names_to = "dg_num", values_to = "cd") |> 
  drop_na(cd) |> 
  mutate(oud_hillary_dt = case_when(cd %in% codes ~ SRVC_BGN_DT)) |>  
  drop_na(oud_hillary_dt) |> 
  distinct(BENE_ID, oud_hillary_dt)

oth_hillary <-
  select(oth, BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |> 
  inner_join(cohort) |> 
  collect() |> 
  filter(!(is.na(DGNS_CD_1) & is.na(DGNS_CD_2))) |> 
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |> 
  # define oud variable of interest via codes
  mutate(oud_hillary_dt = case_when(DGNS_CD_1 %in% codes ~ SRVC_BGN_DT,
                                    DGNS_CD_2 %in% codes ~ SRVC_BGN_DT)) |>
  drop_na(oud_hillary_dt) |> # drop anyone who doesn't have a date for the oud var of interest
  distinct(BENE_ID, oud_hillary_dt)

oud_hillary <- 
  bind_rows(iph_hillary, oth_hillary) |> 
  distinct()

oud_hillary <- 
  inner_join(oud_hillary, cohort) |> 
  filter(oud_hillary_dt %within% interval(washout_start_dt, exposure_period_end_dt + 455))

write_data(oud_hillary, paste0("msk_washout_continuous_enrollment_opioid_requirements_oud_hillary_dts", i, ".fst"))
}
