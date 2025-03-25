# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
#devtools::install_github("nt-williams/riesznet", auth_token = "TOKEN", force = TRUE)
library(lmtp, lib.loc = "/mnt/dev/lmtp-local-riesznet")
library(riesznet)
library(tidyverse)
library(mlr3superlearner)
library(mlr3extralearners)
library(glmnet)
library(earth)
library(lightgbm)
library(ranger)

drv_root <- "/mnt/general-data/disability/everything-local-lmtp/"

df <- fst::read_fst(paste0(drv_root ,"msk_cohort_clean_imputed.fst")) |>
  mutate(oud_period_5 = case_when(oud_period_4 == 1 ~ 1,
                                  cens_period_5 == 0 ~ as.numeric(NA),
                                  TRUE ~ oud_period_5))

fst::write_fst(df, paste0(drv_root ,"msk_cohort_clean_imputed.fst"))


