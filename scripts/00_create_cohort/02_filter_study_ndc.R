# -------------------------------------
# Script: 02_filter_study_ndc.R
# Author: Nick Williams
# Updated:
# Purpose: Export unique NDC for beneficiaries during the baseline/exposure time frame. 
# Notes:
# -------------------------------------

library(arrow)
library(fst)
library(data.table)
library(lubridate)
library(dplyr)

source("R/helpers.R")

# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in OTL (Other services line) 
otl <- open_otl()

cohort <- load_data("msk_washout_continuous_enrollment_dts.fst")
cohort[, let(exposure_end_dt = msk_diagnosis_dt + days(91))]

# OTL ---------------------------------------------------------------------

# Filter OTL to beneficiaries in the MSK cohort
otl <- 
  otl |> 
  select(all_of(c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC"))) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  mutate(LINE_SRVC_BGN_DT = ifelse(is.na(LINE_SRVC_BGN_DT), 
                                    LINE_SRVC_END_DT, 
                                    LINE_SRVC_BGN_DT)) |> 
  filter((LINE_SRVC_BGN_DT >= washout_start_dt) & (LINE_SRVC_BGN_DT <= exposure_end_dt)) |> 
  select(NDC) |> 
  distinct()

otl <- collect(otl) |> 
  as.data.table()

# RXL ---------------------------------------------------------------------

# Filter RXL to beneficiaries in the MSK cohort
rxl <- 
  rxl |> 
  select(all_of(c("BENE_ID", "CLM_ID", "RX_FILL_DT", "NDC"))) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  filter((RX_FILL_DT >= washout_start_dt) & (RX_FILL_DT <= exposure_end_dt)) |> 
  select(NDC) |> 
  distinct()

rxl <- collect(rxl) |> 
  as.data.table()

# combine -----------------------------------------------------------------

study_ndc <- 
  rbind(otl, rxl) |> 
  unique() |> 
  na.omit()

write_data(study_ndc, "study_period_unique_ndc.fst", "data/public")
