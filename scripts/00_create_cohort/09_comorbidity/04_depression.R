################################################################################
################################################################################
###  CREATE DEPRESSION VARIABLES
###  Kat Hoffman, March 2023
###  Purpose: clean TAFOTH and TAFIPH files for depression ICD codes
###  Output: cleaned data file containing minimum date the beneficiary ("data/final/depression.rds")
###        has a depression ICD code in the study duration
###         and indicators of whether it occurs in washout or overall study duration
################################################################################
################################################################################

# Set up -----------------------------------------------------------------------

# load libraries
library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(tictoc)

set.seed(9)

source(here::here("R/helpers.R"))

# Readin OTH and IPH as arrow datsets -----------------------------------------------------------------------

src_root <- "/mnt/processed-data/disability"
files <- paste0(list.files(src_root, pattern = "*TAFOTH*", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
oth <- open_dataset(file.path(src_root, parquet_files))

files <- paste0(list.files(src_root, pattern = "*TAFIPH*", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
iph <- open_dataset(file.path(src_root, parquet_files))

# read in cohort dates file
dts_cohorts <- load_data("msk_cohort.fst")

# read in all icd depression codes
depression_icds <- read_csv("~/disability/projects/create_cohort/input/ICD_codes/depression_icd10_20230323.csv", col_names = F) |>
  rename(ICD9_OR_10 = X1)

############################################################################
############################################################################
# Step 1: across the 17 beneficiary splits, extract OTH codes and 
#       keep only the diagnosis codes (1 and 2, separately) which are in the depression
############################################################################
############################################################################

ids <- dts_cohorts |> pull(BENE_ID)
dg1 <- 
  oth |> 
  filter(BENE_ID %in% ids) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, DGNS_CD_1) |>
  rename(dgcd = DGNS_CD_1) |>
  filter(dgcd %in% depression_icds$ICD9_OR_10) |>
  arrange(SRVC_BGN_DT) |>
  collect() 
dg2 <- 
  oth |> 
  filter(BENE_ID %in% ids) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, DGNS_CD_2) |>
  rename(dgcd = DGNS_CD_2) |>
  filter(dgcd %in% depression_icds$ICD9_OR_10) |>
  arrange(SRVC_BGN_DT) |>
  collect()
all_dg <- bind_rows(dg1, dg2)

rm(dg1)
rm(dg2)

############################################################################
############################################################################
# Step 2: across the 17 beneficiary splits, extract OTH codes and 
#       that occur after the washout period begins, and only keep the minimum
############################################################################
############################################################################

all_dg_clean_function  <- function(data, x)
{  
  num_days_start <- days(case_when(
    x == 0 ~ 0,
    x == 1 ~ 30,
    x == 2 ~ 60,
    x == 3 ~ 90,
    x == 4 ~ 121,
    x == 5 ~ 151,
    x == 6 ~ 181
  ))
  
  num_days_end <- days(case_when(
    x == 0 ~ 0,
    x == 1 ~ 30,
    x == 2 ~ 60,
    x == 3 ~ 91,
    x == 4 ~ 121,
    x == 5 ~ 151,
    x == 6 ~ 182
  ))
  
  data |>
    left_join(dts_cohorts |> select(BENE_ID, washout_start_dt, washout_end_dt)) |>
    group_by(BENE_ID) |>
    filter(SRVC_BGN_DT <= washout_end_dt + num_days_end,
           SRVC_END_DT >= washout_start_dt + num_days_start) |>
    mutate(SRVC_BGN_DT = ifelse(SRVC_BGN_DT < washout_start_dt + num_days_start, washout_start_dt + num_days_start, as.Date(SRVC_BGN_DT))) |>
    mutate(SRVC_BGN_DT = as.Date(SRVC_BGN_DT)) |>
    summarize(!!paste0("min_depression_dt", "_", x) := min(SRVC_BGN_DT)) |>
    ungroup()
}

results <- map(0:0, ~all_dg_clean_function(all_dg,  .x))

all_dg_clean <- reduce(results,
                       ~full_join(.x, .y))


############################################################################
############################################################################
# Step 3: extract depression ICD codes from the Inpatient Hospital files
############################################################################
############################################################################
icd_codes_to_check <-
  iph |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
  collect()

iph_dg_clean_function  <- function(data, x)
{ 
  num_days_start <- days(case_when(
    x == 0 ~ 0,
    x == 1 ~ 30,
    x == 2 ~ 60,
    x == 3 ~ 90,
    x == 4 ~ 121,
    x == 5 ~ 151,
    x == 6 ~ 181
  ))
  
  num_days_end <- days(case_when(
    x == 0 ~ 0,
    x == 1 ~ 30,
    x == 2 ~ 60,
    x == 3 ~ 91,
    x == 4 ~ 121,
    x == 5 ~ 151,
    x == 6 ~ 182
  ))
  
  data |>
    mutate(depression = +(if_any(starts_with("DGNS_CD"),  ~. %in% depression_icds$ICD9_OR_10))) |>
    filter(depression == T) |> # only keep depression codes
    left_join(dts_cohorts |> select(BENE_ID, washout_start_dt, washout_end_dt)) |> # join washout start date in
    group_by(BENE_ID) |>
    filter(SRVC_BGN_DT <= washout_end_dt + num_days_end,
           SRVC_END_DT >= washout_start_dt + num_days_start) |>
    mutate(SRVC_BGN_DT = ifelse(SRVC_BGN_DT < washout_start_dt + num_days_start, washout_start_dt + num_days_start, as.Date(SRVC_BGN_DT))) |>
    mutate(SRVC_BGN_DT = as.Date(SRVC_BGN_DT)) |>
    summarize(!!paste0("min_depression_dt", "_", x, "_iph") := min(SRVC_BGN_DT))
}

results <- map(0:0, ~iph_dg_clean_function(icd_codes_to_check,  .x))

iph_dg <- reduce(results,
                 ~full_join(.x, .y))
############################################################################
############################################################################
# Step 4: across the 17 OTH splits, left join the IPH file
#   keep only the minimum depression date between OTH and IPH for that beneficiary
############################################################################
############################################################################

# all the cleaned files (all minimum dates except beneficiaries that only occur in IPH)
all_depression_oth <- all_dg_clean |>
  left_join(iph_dg) |>
  mutate(min_depression_dt_0 = pmax(min_depression_dt_0, min_depression_dt_0_iph, na.rm = TRUE)
  ) |>
  select(BENE_ID, min_depression_dt_0)

# iph_dg <- read_parquet("data/tafiph/depression_iph.parquet") |> collect()
############################################################################
############################################################################
# Step 5: add in beneficiaries minimum dates that were only in IPH, not OTH
############################################################################
############################################################################

# pull out beneficiaries that we don't already have in OTH
iph_only <-
  iph_dg |>
  filter(!(BENE_ID %in% all_depression_oth$BENE_ID)) |>
  rename(min_depression_dt_0 = min_depression_dt_0_iph
  )

# bind all the rows together (bene_id, depression_dt)
all_depression <-
  bind_rows(all_depression_oth, iph_only) #|>
#arrange(depression_dt) |>
# distinct(BENE_ID, .keep_all = T)

############################################################################
############################################################################
# Step 6: add indicators for when the minimum date of depression occurred
############################################################################
############################################################################

all_depression_clean <- 
  dts_cohorts |>
  left_join(all_depression) |>
  mutate(depression_washout_6mos_cal = case_when(min_depression_dt_0 %within% interval(washout_start_dt, washout_end_dt) ~ 1,
                                                 TRUE ~ 0)) |>
  select(BENE_ID, min_depression_dt_0,
         depression_washout_6mos_cal)

write_rds(all_depression_clean, "/mnt/general-data/disability/everything-local-lmtp/comorbidity/depression.rds") # save final data file
