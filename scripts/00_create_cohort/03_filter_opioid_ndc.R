# -------------------------------------
# Script: 04_filter_opioid_ndc.R
# Author: Nick Williams
# Purpose: Filter to observations with no opioids in washout but opioids in the exposure period
# Updated: Shodai (March 25 2025) -- updated to find minimum opioid date post MSK diagnosis
# Notes:
# -------------------------------------

library(data.table)
library(tidyverse)
library(yaml)
library(foreach)
library(fst)
library(arrow)

source("R/helpers.R")

ndc <- readRDS("data/public/ndc_to_atc_crosswalk.rds")
codes <- read_yaml("data/public/drug_codes.yml")

# load initial continuous enrollment cohort
cohort <- load_data("msk_washout_dts.fst") |>
  as.data.table() |>
  mutate(opioid_start_dt_possible_latest = msk_diagnosis_dt + days(90))

# find opioid ndcs --------------------------------------------------------

opioids <- names(codes[["Opioid pain"]]$ATC)

opioid_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
  any(sapply(opioids, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc_opioids <- ndc[opioid_flag]

saveRDS(ndc_opioids, "data/public/ndc_to_atc_opioids.rds")

ndc_opioids <- readRDS("data/public/ndc_to_atc_opioids.rds")


# filter rxl and otl files ------------------------------------------------
# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in OTL (Other services line) 
otl <- open_otl()

# Find beneficiaries with an opioid following MSK in OTL
otl <- 
  select(otl, BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, NDC) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  mutate(LINE_SRVC_BGN_DT = ifelse(
    is.na(LINE_SRVC_BGN_DT), 
    LINE_SRVC_END_DT, 
    LINE_SRVC_BGN_DT
  )) |> 
  filter((LINE_SRVC_BGN_DT >= msk_diagnosis_dt) & 
           (LINE_SRVC_BGN_DT <= opioid_start_dt_possible_latest), 
         NDC %in% ndc_opioids$NDC,
         !(NDC %in% c("27505005036", "27505005096"))) |> # lucemyra -- not an opioid
  select(BENE_ID, LINE_SRVC_BGN_DT) |>
  distinct()

otl <- collect(otl) |> as.data.table()

# Find beneficiaries with an opioid following MSK in OTL
rxl <- 
  #select(rxl, BENE_ID, CLM_ID, RX_FILL_DT, NDC) |> 
  rxl |>
  inner_join(cohort, by = "BENE_ID") |> 
  filter((RX_FILL_DT >= msk_diagnosis_dt) & 
           (RX_FILL_DT <= opioid_start_dt_possible_latest), 
         NDC %in% ndc_opioids$NDC,
         !(NDC %in% c("27505005036", "27505005096"))) |> # lucemyra -- not an opioid
  #select(BENE_ID, RX_FILL_DT) |> 
  distinct()

rxl <- collect(rxl) |> as.data.table()

otl_grouped <- otl |>
  group_by(BENE_ID) |>
  summarize(min_opioid_date = min(LINE_SRVC_BGN_DT))

rxl_grouped <- rxl |>
  group_by(BENE_ID) |>
  summarize(min_opioid_date = min(RX_FILL_DT))

# Combine and export
keep <- unique(rbind(otl_grouped, rxl_grouped)) |>
  group_by(BENE_ID) |>
  summarize(min_opioid_date = min(min_opioid_date))

cohort_with_opioids <- unique(left_join(keep, cohort))

# number removed for no opioids in 3 month period following MSK
nrow(cohort) - nrow(keep)

cohort_with_opioids <- cohort_with_opioids |>
  mutate(washout_start_dt = min_opioid_date - days(182),
         washout_end_dt = min_opioid_date - days(1),
         exposure_end_dt_possible_latest = min_opioid_date + days(90)) |>
  select(BENE_ID, washout_start_dt, msk_diagnosis_dt, washout_end_dt, min_opioid_date, exposure_end_dt_possible_latest)

### LOOKING FOR OPIOIDS IN WASHOUT

# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in OTL (Other services line) 
otl <- open_otl()

# Find beneficiaries with an opioid prior to MSK diagnosis in OTL (washout)
otl <- 
  select(otl, BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, NDC) |> 
  inner_join(cohort_with_opioids, by = "BENE_ID") |> 
  mutate(LINE_SRVC_BGN_DT = ifelse(
    is.na(LINE_SRVC_BGN_DT), 
    LINE_SRVC_END_DT, 
    LINE_SRVC_BGN_DT),
    LINE_SRVC_END_DT = ifelse(
      is.na(LINE_SRVC_END_DT), 
      LINE_SRVC_BGN_DT, 
      LINE_SRVC_END_DT),
    LINE_SRVC_BGN_DT = ifelse(LINE_SRVC_BGN_DT < washout_start_dt & LINE_SRVC_END_DT >= washout_start_dt, washout_start_dt, LINE_SRVC_BGN_DT) # if service began before but ends after washout, then consider start as washout
  ) |> 
  filter((LINE_SRVC_BGN_DT < msk_diagnosis_dt & (LINE_SRVC_BGN_DT >= washout_start_dt)), 
         NDC %in% ndc_opioids$NDC) |> 
  select(BENE_ID) |> 
  distinct()

otl <- collect(otl) |> as.data.table()

# Find beneficiaries with an opioid prior to MSK diagnosis in RXL (washout)
rxl <- 
  select(rxl, BENE_ID, CLM_ID, RX_FILL_DT, DAYS_SUPPLY, NDC) |> 
  inner_join(cohort_with_opioids, by = "BENE_ID") |> 
  collect() |>
  mutate(RX_END_DT = RX_FILL_DT + DAYS_SUPPLY - 1,
         RX_FILL_DT = ifelse(RX_FILL_DT < washout_start_dt & RX_END_DT >= washout_start_dt, washout_start_dt, RX_FILL_DT) # if service began before but ends after washout, then consider start as washout
  ) |>
  select(-c(DAYS_SUPPLY)) |>
  filter((RX_FILL_DT < msk_diagnosis_dt & (RX_FILL_DT >= washout_start_dt)), 
         NDC %in% ndc_opioids$NDC) |> 
  select(BENE_ID) |> 
  distinct()

rxl <- rxl |> as.data.table()

# remove observations with opioid in washout period
remove <- rbind(otl, rxl) |> unique()

# number of patients with opioids prior to MSK diagnosis
remove |> nrow()

cohort_with_opioids_none_in_washout <- anti_join(cohort_with_opioids, remove)

write_data(cohort_with_opioids_none_in_washout, "msk_washout_opioid_requirements.fst")
