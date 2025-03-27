# -------------------------------------
# Script: 03_classify_study_ndc.R
# Author: Nick Williams
# Updated: 
# Purpose: Classify unique NDC using ATC
# Notes: Modified from https://github.com/CI-NYC/rxnorm-paper/blob/main/scripts/00_pipeline.R
# -------------------------------------

library(rxnorm)
library(data.table)
library(purrr)
library(stringr)
library(foreach)
library(readr)
library(glue)
library(doFuture)
library(fst)

local <- FALSE

# Load list of NDCs
ndc <- read_fst("data/public/study_period_unique_ndc.fst") |>
  as.data.table()

# Convert NDC -> RxCUI -> ATC
plan(multisession, workers = 10)

ndc_status <- foreach(code = ndc[, NDC]) %dofuture% {
  get_ndc_status(code, local_host = local)
}

ndc[, ndc_status := unlist(ndc_status)]

unknown <- ndc[ndc_status == "UNKNOWN" | is.na(ndc_status), ]
ndc <- ndc[ndc_status %in% c("ACTIVE", "OBSOLETE", "ALIEN")]

drug_classes <- foreach(code = ndc$NDC) %dofuture% {
  rxcui <- from_ndc(code, local_host = local)
  atc <- get_atc(rxcui, local_host = local)
  list(rxcui = rxcui, atc = atc)
}

ndc[, `:=`(rxcui = map_chr(drug_classes, "rxcui"), 
           atc = map(drug_classes, "atc"))]

obsolete <- ndc[is.na(atc) & !is.na(rxcui), ]

rxcui_status <- foreach(code = obsolete[, rxcui]) %dofuture% {
  get_rxcui_status(code, local_host = local)
}

obsolete[, rxcui_status := unlist(rxcui_status)]

new_rxcui <- 
  foreach(code = obsolete[, rxcui], 
          status = obsolete[, rxcui_status]) %dofuture% {
            if (status %in% c("Remapped", "NotCurrent")) {
              return(get_remapped_rxcui(code, local_host = local))
            }
            
            if (status == "Obsolete") {
              return(get_scd_rxcui(code, local_host = local))
            }
            
            if (status == "Quantified") {
              return(get_quantified_rxcui(code, local_host = local))
            }
            
            return(code)
          }

obsolete[, rxcui := fifelse(!is.na(unlist(new_rxcui)), unlist(new_rxcui), rxcui)]

alt_drug_class <- foreach(code = unlist(new_rxcui)) %dofuture% {
  if (is.na(code)) return(NA_character_)
  get_atc(code, local_host = local)
}

obsolete[, atc := alt_drug_class]

ndc <- rbind(ndc[!(NDC %in% obsolete$NDC), ], 
             obsolete[, .(NDC, ndc_status, rxcui, atc, rxcui_status)], 
             fill = TRUE)

unclassified <- ndc[is.na(atc) & !is.na(rxcui), ]

rxname <- foreach(code = unclassified[, rxcui]) %dofuture% {
  get_rx(code, local_host = local)
}

unclassified[, rxname := rxname]

saveRDS(ndc, "data/public/ndc_to_atc_crosswalk.rds")