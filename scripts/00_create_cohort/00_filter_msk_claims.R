# -------------------------------------
# Script: 00_filter_msk_claims.R
# Author: Nick Williams
# Updated: Shodai (March 24 2025) -- updated possible MSK diagnosis dates to 01/01/2016 - 12/31/2019
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

source("R/helpers.R")

# Load necessary datasets
oth <- open_oth()
iph <- open_iph()

codes <- read_yaml("data/public/msk_codes.yml")$code

start_dt <- as.Date("2016-01-01")
end_dt <- as.Date("2019-12-31")

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
                   .SD, 
                   .SDcols = c("BENE_ID", "CLM_ID", "SRVC_BGN_DT", paste0("DGNS_CD_", 1:10))]

setkey(oth_msk, BENE_ID, CLM_ID)
oth_msk <- unique(oth_msk, by = c(1, 2))

setkey(iph_msk, BENE_ID, CLM_ID)
iph_msk <- unique(iph_msk, by = c(1, 2))

# Remove rows with missing BENE_ID
oth_msk <- oth_msk[!is.na(BENE_ID)]
iph_msk <- iph_msk[!is.na(BENE_ID)]

# Sort by date and take first claim for each BENE_ID to calculate the earliest possible washout start date
setorder(oth_msk, BENE_ID, SRVC_BGN_DT)
oth_msk[, let(washout_start_dt_possible_earliest = min(SRVC_BGN_DT)), BENE_ID]
oth_msk <- oth_msk[SRVC_BGN_DT == washout_start_dt_possible_earliest]
oth_msk[, let(washout_start_dt_possible_earliest = washout_start_dt_possible_earliest - days(182))]

setorder(iph_msk, BENE_ID, SRVC_BGN_DT)
iph_msk[, let(washout_start_dt_possible_earliest = min(SRVC_BGN_DT)), BENE_ID]
iph_msk <- iph_msk[SRVC_BGN_DT == washout_start_dt_possible_earliest]
iph_msk[, let(washout_start_dt_possible_earliest = washout_start_dt_possible_earliest - days(182))]

oth_msk <- unique(oth_msk[, .(BENE_ID, washout_start_dt_possible_earliest, SRVC_BGN_DT)], by = 1)
iph_msk <- unique(iph_msk[, .(BENE_ID, washout_start_dt_possible_earliest, SRVC_BGN_DT)], by = 1)

msk <- rbindlist(list(oth_msk, iph_msk))
msk[, let(washout_start_dt_possible_earliest_comb = min(washout_start_dt_possible_earliest)), BENE_ID]
msk <- msk[washout_start_dt_possible_earliest == washout_start_dt_possible_earliest_comb, .(BENE_ID, washout_start_dt_possible_earliest, SRVC_BGN_DT)]

setnames(msk, "SRVC_BGN_DT", "msk_diagnosis_dt")

write_data(as.data.table(distinct(msk)), "msk_washout_dts.fst")

# number of people with MSK pain claims
msk |> nrow() #4,227,282
