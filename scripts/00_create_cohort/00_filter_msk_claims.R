# -------------------------------------
# Script: filter_msk_claims.R
# Author: Nick Williams
# Purpose: Find all msk claims within the specified date range 
#   in the Other Services and Inpatient files
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)
library(fst)

src_root <- "/mnt/processed-data/disability"

codes <- read_yaml("data/public/msk_codes.yml")$code
start_dt <- as.Date("2016-07-01")
end_dt <- as.Date("2019-10-01")

oth <- 
  list.files(src_root, 
             pattern = "TAFOTH\\d+_\\d+\\.parquet", 
             recursive = TRUE) |> 
  (\(files) file.path(src_root, files))() |> 
  open_dataset()

iph <- 
  list.files(src_root, 
             pattern = "TAFIPH_\\d+\\.parquet", 
             recursive = TRUE) |> 
  (\(files) file.path(src_root, files))() |> 
  open_dataset()

keep <- c("BENE_ID", 
          "CLM_ID", 
          "SRVC_BGN_DT", 
          "SRVC_END_DT", 
          paste0("DGNS_CD_", 1:10))

oth_msk <- 
  select(oth, any_of(keep)) |> 
  filter(DGNS_CD_1 %in% codes | DGNS_CD_2 %in% codes) |>
  collect() |> 
  as.data.table()

iph_msk <- 
  select(iph, any_of(keep)) |> 
  filter(DGNS_CD_1 %in% codes | 
           DGNS_CD_2 %in% codes | 
           DGNS_CD_3 %in% codes | 
           DGNS_CD_4 %in% codes | 
           DGNS_CD_5 %in% codes | 
           DGNS_CD_6 %in% codes | 
           DGNS_CD_7 %in% codes | 
           DGNS_CD_8 %in% codes | 
           DGNS_CD_9 %in% codes | 
           DGNS_CD_10 %in% codes) |>
  collect() |> 
  as.data.table()

oth_msk[, SRVC_BGN_DT := fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)]

oth_msk <- oth_msk[SRVC_BGN_DT %within% interval(start_dt, end_dt), 
                   .(BENE_ID, CLM_ID, SRVC_BGN_DT, DGNS_CD_1, DGNS_CD_2)]

iph_msk <- iph_msk[SRVC_BGN_DT %within% interval(start_dt, end_dt), 
                   .SD, .SDcols = c("BENE_ID", "CLM_ID", "SRVC_BGN_DT", paste0("DGNS_CD_", 1:10))]

setkey(oth_msk, BENE_ID, CLM_ID)
oth_msk <- unique(oth_msk, by = c(1, 2))

setkey(iph_msk, BENE_ID, CLM_ID)
iph_msk <- unique(iph_msk, by = c(1, 2))

# Remove rows with missing BENE_ID
oth_msk <- oth_msk[!is.na(BENE_ID)]
iph_msk <- iph_msk[!is.na(BENE_ID)]

# Sort by date and take first claim for each BENE_ID to calculate washout start date
setorder(oth_msk, BENE_ID, SRVC_BGN_DT)
oth_msk[, let(washout_start_dt = min(SRVC_BGN_DT)), BENE_ID]
oth_msk <- oth_msk[SRVC_BGN_DT == washout_start_dt]
oth_msk[, let(washout_start_dt = washout_start_dt - days(182))]

setorder(iph_msk, BENE_ID, SRVC_BGN_DT)
iph_msk[, let(washout_start_dt = min(SRVC_BGN_DT)), BENE_ID]
iph_msk <- iph_msk[SRVC_BGN_DT == washout_start_dt]
iph_msk[, let(washout_start_dt = washout_start_dt - days(182))]

oth_msk <- unique(oth_msk[, .(BENE_ID, washout_start_dt, SRVC_BGN_DT)], by = 1)
iph_msk <- unique(iph_msk[, .(BENE_ID, washout_start_dt, SRVC_BGN_DT)], by = 1)

msk <- rbindlist(list(oth_msk, iph_msk))
msk[, let(washout_start_dt_comb = min(washout_start_dt)), BENE_ID]
msk <- msk[washout_start_dt == washout_start_dt_comb, .(BENE_ID, washout_start_dt, SRVC_BGN_DT)]

setnames(msk, "SRVC_BGN_DT", "msk_diagnosis_dt")

write_fst(msk, "/mnt/general-data/disability/everything-local-lmtp/msk_washout_dts.fst")
