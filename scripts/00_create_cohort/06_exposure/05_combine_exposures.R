# -------------------------------------
# Script: 05_combine_exposures.R
# Author: Nick Williams
# Purpose: Combine two exposures and define exposure subsets
# Notes:
# -------------------------------------

library(collapse)
library(fst)

source("R/helpers.R")

mme <- load_data("exposure_max_daily_dose_mme.fst")
days_supply <- load_data("exposure_days_supply.fst")

exposures <- 
  join(mme, days_supply, how = "full") |> 
  fmutate(subset_B1 = exposure_max_daily_dose_mme >= 50, 
          subset_B2 = exposure_days_supply > 7, 
          subset_B3 = subset_B1 & subset_B2)

write_data(exposures, "exposures_with_subsets.fst")
