# -------------------------------------
# Script: 01_01_filter_continuous_enrollment.R
# Author: Nick Williams
# Purpose: Split enrollment periods into chunks per beneficiary
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(doFuture)
library(dplyr)

src_root <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/everything-local-lmtp"

# Load washout dates
washout <- read_fst(file.path(drv_root, "msk_washout_dts.fst"), 
                    as.data.table = TRUE)

washout[, let(exposure_end_dt = msk_diagnosis_dt + days(91))]

# Load all dates
dates <- 
  list.files(src_root, 
           pattern = "TAFDEDTS_\\d+\\.parquet", 
           recursive = TRUE) |> 
  (\(files) file.path(src_root, files))() |> 
  open_dataset()

dates <- 
  filter(dates, !is.na(BENE_ID)) |> 
  select(BENE_ID, ENRLMT_START_DT, ENRLMT_END_DT) |>
  inner_join(washout, by = "BENE_ID") |> 
  collect()

setDT(dates, key = "BENE_ID")
dates <- dates[order(rleid(BENE_ID), ENRLMT_START_DT)]

dates <- dates[!is.na(ENRLMT_START_DT) & !is.na(ENRLMT_END_DT)]

dates <- dates[ENRLMT_START_DT <= exposure_end_dt]

idx <- split(seq_len(nrow(dates)), list(dates$BENE_ID))
tmp <- lapply(idx, \(x) dates[x])

rm(idx, washout, dates)
gc()

# Define the function to split a list into chunks
split_list_into_chunks <- function(lst, chunk_size) {
  split(seq_along(lst), ceiling(seq_along(lst) / chunk_size))
}

chunks <- split_list_into_chunks(tmp, 1e5)

# Save each chunk to a separate RDS file
for (i in seq_along(chunks)) {
  file_name <- paste0(file.path(drv_root, "tmp"), '/enrollment_period_chunk_', i, '.rds')
  saveRDS(tmp[chunks[[i]]], file = file_name)
}
