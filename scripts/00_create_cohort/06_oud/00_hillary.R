# -------------------------------------
# Script: 00_hillary.R
# Author: Nick Williams
# Purpose:
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

cohort <- read_fst(
  "/mnt/general-data/disability/everything-local-lmtp/msk_washout_continuous_enrollment_opioid_requirements.fst", 
  as.data.table = TRUE
)

# Read in IPH dataset
iph <- open_iph()

# read in OTH 
oth <- open_oth()

# Lets keep all OUD codes from washout period until end of followup period..

iph_hillary <- iph |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
  collect() |> 
  inner_join(cohort) |> 
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |> 
  # switch to long format data because there are 12 dx codes and IPH isn't too big
  pivot_longer(cols = contains("DGNS_CD"), names_to = "dg_num", values_to = "cd") |> 
  # drop any missing icd code (cd) rows
  drop_na(cd) |> 
  # define oud variable of interest via codes
  mutate(oud_hillary_dt = case_when(cd %in% codes ~ SRVC_BGN_DT)) |>  
  drop_na(oud_hillary_dt) |> 
  # drop anyone who doesn't have a date for the oud var of interest
  distinct(BENE_ID, oud_hillary_dt)

oth_hillary <- oth |> 
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |> 
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
  filter(oud_hillary_dt %within% interval(washout_start_dt, exposure_end_dt + 455))

write_fst(
  oud_hillary, 
  file.path(
    "/mnt/general-data/disability/everything-local-lmtp", 
    "msk_washout_continuous_enrollment_opioid_requirements_oud_hillary_dts.fst"
  )
)
