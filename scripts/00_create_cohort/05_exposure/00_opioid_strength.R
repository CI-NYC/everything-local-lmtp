# -------------------------------------
# Script: 00_opioid_strength.R
# Author: Nick Williams
# Updated:
# Purpose: Add strength and dose form to NDC lookup table for opioids
# Notes:
# -------------------------------------

library(data.table)
library(rxnorm)
library(foreach)
library(doFuture)
library(furrr)

opioids <- readRDS("data/public/ndc_to_atc_opioids.rds")

plan(multisession, workers = 10)

strength <- foreach(code = opioids[, rxcui]) %dofuture% {
  get_rxcui_strength(code, local_host = TRUE)
}

opioids[, strength := strength]
opioids[, dose_form := future_map_chr(opioids$rxcui, get_dose_form, local_host = TRUE)]

plan(sequential)

saveRDS(opioids, "data/public/ndc_to_atc_opioids_with_strength.rds")
