# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(doFuture)
library(dplyr)
library(tidyr)

src_root <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/everything-local-lmtp/"

# Load washout dates
washout <- read_fst(file.path(drv_root, "msk_washout_dts.fst"), 
                    as.data.table = TRUE)

# Load demographics dataset
demo_dataset <- 
  list.files(src_root, 
             pattern = "TAFDEBSE_\\d+\\.parquet", 
             recursive = TRUE) |> 
  (\(files) file.path(src_root, files))() |> 
  open_dataset()

# Load eligibilty codes
codes <- read_yaml("data/public/eligibility_codes.yml")

# age, sex, state ---------------------------------------------------------

# Merge with washout
demo <- 
  select(demo_dataset, BENE_ID, STATE_CD, BIRTH_DT, SEX_CD) |>
  right_join(washout, by = "BENE_ID") |> 
  collect()

setDT(demo)

# Filter out MD
demo <- demo[STATE_CD != "MD"]
# Remove rows missing sex
demo <- demo[!is.na(SEX_CD)]
# Calculate age at washout start and filter to age range
demo[, let(washout_age = as.numeric(difftime(washout_start_dt, BIRTH_DT, "days")) / 365.25)]
demo <- demo[washout_age >= 19 & washout_age <= 65]

demo <- unique(demo)

# eligibility codes -------------------------------------------------------

# Query eligibility codes and perform merge with valid demo ids
eligibility_codes <-
  select(demo_dataset, BENE_ID, RFRNC_YR, starts_with("ELGBLTY_GRP_CD")) |>
  right_join(demo[, .(BENE_ID)], by = "BENE_ID") |> 
  arrange(BENE_ID, RFRNC_YR) |>
  collect()

eligibility_codes <- 
  select(eligibility_codes, -ELGBLTY_GRP_CD_LTST) |>
  pivot_longer(cols = starts_with("ELGBLTY_GRP_CD"),
               names_to = "month",
               values_to = "elig_code",
               values_drop_na = TRUE) |>
  mutate(month = readr::parse_number(month),
         year = as.numeric(RFRNC_YR),
         elig_dt = as.Date(paste0(year, "-", month, "-01")))

eligibility_codes <- 
  left_join(eligibility_codes, washout[, .(BENE_ID, washout_start_dt, msk_diagnosis_dt)]) |> 
  filter(elig_dt %within% interval(washout_start_dt, msk_diagnosis_dt)) |>
  arrange(elig_dt) |>
  group_by(BENE_ID) |>
  filter(row_number() == n()) |>
  select(BENE_ID, washout_elig_dt = elig_dt, washout_elig_code = elig_code)

`%nin%` <- function(x, y) !(x %in% y)

setDT(eligibility_codes)

eligibility_codes <- eligibility_codes[
  washout_elig_code %nin% codes$pregnant & 
    washout_elig_code %nin% codes$institution & 
    washout_elig_code %nin% codes$unknown_disability  & 
    washout_elig_code %nin% codes$not_eligible  & 
    washout_elig_code %nin% codes$deaf_blind
  ]



