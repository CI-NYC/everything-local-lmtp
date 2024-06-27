# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(tidyverse)
library(yaml)
library(foreach)
library(fst)
library(arrow)

src_root <- "/mnt/processed-data/disability"
ndc <- readRDS("data/public/ndc_to_atc_crosswalk.rds")
codes <- read_yaml("data/public/drug_codes.yml")

cohort <- read_fst(
  "/mnt/general-data/disability/everything-local-lmtp/msk_washout_continuous_enrollment_dts.fst", 
  as.data.table = TRUE
)

cohort[, let(exposure_end_dt = msk_diagnosis_dt + days(91))]

# find opioid ndcs --------------------------------------------------------

opioids <- names(codes[["Opioid pain"]]$ATC)

opioid_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
  any(sapply(opioids, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc_opioids <- ndc[opioid_flag]

saveRDS(ndc_opioids, "data/public/ndc_to_atc_opioids.rds")

# filter rxl and otl files ------------------------------------------------

# Read in RXL (pharmacy line)
files <- paste0(list.files(src_root, pattern = "TAFRXL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
rxl_dataset <- open_dataset(file.path(src_root, parquet_files))

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl_dataset <- open_dataset(file.path(src_root, parquet_files))

# Find beneficiaries with an opioid in the washout period in OTL
otl <- 
  otl_dataset |> 
  select(all_of(c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC"))) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  mutate(LINE_SRVC_BGN_DT = ifelse(is.na(LINE_SRVC_BGN_DT), 
                                   LINE_SRVC_END_DT, 
                                   LINE_SRVC_BGN_DT)) |> 
  filter((LINE_SRVC_BGN_DT >= washout_start_dt) & 
           (LINE_SRVC_BGN_DT <= msk_diagnosis_dt), 
         NDC %in% ndc_opioids$NDC) |> 
  select(BENE_ID) |> 
  distinct()

otl <- collect(otl) |> as.data.table()

# Find beneficiaries with an opioid in the washout period in RXL
rxl <- 
  rxl_dataset |> 
  select(all_of(c("BENE_ID", "CLM_ID", "RX_FILL_DT", "NDC"))) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  filter((RX_FILL_DT >= washout_start_dt) & 
           (RX_FILL_DT <= msk_diagnosis_dt), 
         NDC %in% ndc_opioids$NDC) |> 
  select(BENE_ID) |> 
  distinct()

rxl <- collect(rxl) |> as.data.table()

remove <- rbind(otl, rxl) |> unique()

# remove observations with opioid in washout period
cohort <- anti_join(cohort, remove)

# Find beneficiaries with an opioid in the exposure period in OTL
otl <- 
  otl_dataset |> 
  select(all_of(c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC"))) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  mutate(LINE_SRVC_BGN_DT = ifelse(is.na(LINE_SRVC_BGN_DT), 
                                   LINE_SRVC_END_DT, 
                                   LINE_SRVC_BGN_DT)) |> 
  filter((LINE_SRVC_BGN_DT > msk_diagnosis_dt) & 
           (LINE_SRVC_BGN_DT <= exposure_end_dt), 
         NDC %in% ndc_opioids$NDC) |> 
  select(BENE_ID) |> 
  distinct()

otl <- collect(otl) |> as.data.table()

# Find beneficiaries with an opioid in the exposure period in RXL
rxl <- 
  rxl_dataset |> 
  select(all_of(c("BENE_ID", "CLM_ID", "RX_FILL_DT", "NDC"))) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  filter((RX_FILL_DT > msk_diagnosis_dt) & 
           (RX_FILL_DT <= exposure_end_dt), 
         NDC %in% ndc_opioids$NDC) |> 
  select(BENE_ID) |> 
  distinct()

rxl <- collect(rxl) |> as.data.table()

# Combine and export
keep <- unique(rbind(otl, rxl))
cohort <- unique(left_join(keep, cohort))

write_fst(
  cohort, 
  "/mnt/general-data/disability/everything-local-lmtp/msk_washout_continuous_enrollment_opioid_requirements.fst", 
)
