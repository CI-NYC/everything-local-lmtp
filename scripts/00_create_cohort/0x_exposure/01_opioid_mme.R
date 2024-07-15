# -------------------------------------
# Script: 01_opioid_mme.R
# Author: Nick Williams
# Updated:
# Purpose: Link MME to opioid ingredients
# Notes: Modified from https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/01_create_treatments/00_opioid_mme.R
# -------------------------------------

library(tidyverse)
library(fuzzyjoin)

opioids <- readRDS("data/public/ndc_to_atc_opioids_with_strength.rds")
mme_conversion <- read_csv("data/public/mme.csv")

ci_str_detect <- function(x, y) str_detect(x, regex(y, ignore_case = TRUE))

opioids_mme <- 
  # filter(opioids, flag_opioid_analgesic == TRUE | flag_opioid_anesthetics == TRUE) |> 
  unnest(opioids, cols = "strength") |> 
  fuzzy_inner_join(mme_conversion, 
                   by = c("activeIngredientName" = "opioid"), 
                   match_fun = ci_str_detect)

# Listed as codeine and dihydrocodeine, but really just dihydrocodeine
remove_codeine <- c("42195084010", "57664041988", "66992084010", "55887045690")

opioids_mme <- filter(opioids_mme, !(NDC %in% remove_codeine & opioid == "codeine"))

# Should only be drugs used for MOUD
no_mme <- anti_join(opioids, opioids_mme, by = "NDC")

unnest(no_mme, cols = "strength") |> 
  select(activeIngredientName) |> 
  unique()

opioids_mme <- opioids_mme |> 
  mutate(
    conversion = case_when(
      str_detect(activeIngredientName, "buprenorphine") &
        str_detect(dose_form, "Film|Tablet") ~ 30,
      str_detect(activeIngredientName, "buprenorphine") &
        str_detect(dose_form, "Trans") ~ 12.6,
      str_detect(activeIngredientName, "buprenorphine") &
        str_detect(dose_form, "Implant|Cartridge|Syringe|Inject") ~ 75,
      str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
        str_detect(dose_form, "Lozenge|Tablet") ~ 130,
      str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
        str_detect(dose_form, "Spray") ~ 160,
      str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
        str_detect(dose_form, "Transdermal") ~ 720,
      str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
        str_detect(dose_form, "Syringe|Injection|Cartridge|Pack") ~ 100,
      TRUE ~ conversion
    )
  )

saveRDS(opioids_mme, "data/public/opioids_mme.rds")
