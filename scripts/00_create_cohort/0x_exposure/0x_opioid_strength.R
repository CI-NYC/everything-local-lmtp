# -------------------------------------
# Script: 
# Author: Nick Williams
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(data.table)
library(rxnorm)
library(foreach)
library(doFuture)

opioids <- readRDS("data/public/ndc_to_atc_opioids.rds")

plan(multisession)

strength <- foreach(code = opioids[, rxcui]) %dofuture% {
  get_rxcui_strength(code, local_host = TRUE)
}

opioids[, strength := strength]

plan(sequential)

saveRDS(opioids, "data/public/ndc_to_atc_opioids_with_strength.rds")