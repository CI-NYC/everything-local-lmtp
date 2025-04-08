# -------------------------------------
# Script: 00_naltrexone
# Author: Nick Williams
# Purpose: Identify MOUD naltexrone periods
# Notes:
#   - 3 week (21 day) grace period is used
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(fst)
library(collapse)
library(yaml)

source("R/helpers.R")

for (i in c("", "_7_day_gap"))
{
  cohort <- load_data(paste0("msk_washout_continuous_enrollment_opioid_requirements_with_exposures", i, ".fst"))
  
bup_list <- read_fst("data/public/bup_list.fst")
hcpcs <- read_yaml("data/public/hcpcs_codes.yml")$naltrexone

# other services line file
otl <- open_otl()
# pharmacy line file
rxl <- open_rxl()

# RXL ---------------------------------------------------------------------

rxl <- 
  filter(rxl, NDC == "65757030001") |> 
  select(BENE_ID, 
         NDC,
         CLM_ID,
         NDC_UOM_CD, 
         NDC_QTY,
         DAYS_SUPPLY,
         RX_FILL_DT) |>
  collect()

rxl_nal <- 
  fsubset(rxl, BENE_ID %in% cohort$BENE_ID) |> 
  fmutate(moud_end_dt = RX_FILL_DT + days(DAYS_SUPPLY - 1 + 21)) |> 
  fselect(BENE_ID, moud_start_dt = RX_FILL_DT, moud_end_dt) |> 
  funique()

# OTL ---------------------------------------------------------------------

# - start with NDC
otl_ndc_nal <- 
  filter(otl, NDC == "65757030001") |> 
  select(BENE_ID,
         CLM_ID,
         NDC,
         NDC_UOM_CD, 
         NDC_QTY,
         LINE_SRVC_BGN_DT,
         LINE_SRVC_END_DT,
         LINE_PRCDR_CD,
         LINE_PRCDR_CD_SYS,
         ACTL_SRVC_QTY,
         ALOWD_SRVC_QTY) |> 
  collect()

# - Assuming 30 day supply
otl_ndc_nal <- 
  fsubset(otl_ndc_nal, BENE_ID %in% cohort$BENE_ID) |> 
  fmutate(LINE_SRVC_BGN_DT = case_when(
    is.na(LINE_SRVC_BGN_DT) ~ LINE_SRVC_END_DT, 
    TRUE ~ LINE_SRVC_BGN_DT
  )) |> 
  fmutate(moud_end_dt = LINE_SRVC_BGN_DT + 21 + 29) |> 
  fselect(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT, moud_end_dt) |> 
  funique()

# - filter down using HCPCS
otl_hcpcs_nal <- 
  filter(otl, LINE_PRCDR_CD %in% hcpcs) |> 
  select(BENE_ID,
         CLM_ID,
         NDC,
         NDC_UOM_CD, 
         NDC_QTY,
         LINE_SRVC_BGN_DT,
         LINE_SRVC_END_DT,
         LINE_PRCDR_CD,
         LINE_PRCDR_CD_SYS,
         ACTL_SRVC_QTY,
         ALOWD_SRVC_QTY) |> 
  collect()

# - Assuming 30 day supply
otl_hcpcs_nal <-
  fsubset(otl_hcpcs_nal, BENE_ID %in% cohort$BENE_ID) |> 
  fmutate(
    LINE_SRVC_BGN_DT = fifelse(is.na(LINE_SRVC_BGN_DT), LINE_SRVC_END_DT, LINE_SRVC_BGN_DT), 
    moud_end_dt = LINE_SRVC_BGN_DT + 21 + 29
  ) |> 
  fselect(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT, moud_end_dt) |> 
  funique()

# combine -----------------------------------------------------------------

nal <- 
  rbindlist(
    list(
      rxl_nal, 
      otl_ndc_nal, 
      otl_hcpcs_nal
    )
  ) |> 
  funique()

# - Save all moud periods for the initial cohort
write_data(nal, paste0("msk_washout_continuous_enrollment_opioid_requirements_moud_nal_intervals", i, ".fst"))

moud_nal <- 
  roworder(nal, BENE_ID, moud_start_dt) |> 
  join(cohort, how = "left") |> 
  fmutate(moud_nal_washout = int_overlaps(
    interval(moud_start_dt, moud_end_dt),
    interval(washout_start_dt, washout_end_dt)
  )) |> 
  fgroup_by(BENE_ID) |> 
  fsummarise(moud_nal_washout = as.numeric(sum(moud_nal_washout) > 0))

# - Rejoin entire initial cohort and save
moud_nal <- 
  join(cohort, moud_nal, how = "left") |> 
  fmutate(moud_nal_washout = replace_na(moud_nal_washout, 0))

write_data(moud_nal, paste0("msk_washout_continuous_enrollment_opioid_requirements_moud_nal_washout", i, ".fst"))
}
