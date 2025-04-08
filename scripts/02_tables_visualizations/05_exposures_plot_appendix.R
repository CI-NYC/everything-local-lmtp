# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(ggplot2)
library(patchwork)
library(tidyverse)

df <- fst::read_fst(paste0(drv_root ,"msk_cohort_clean_imputed.fst")) |>
  as.data.table()

summary(df$exposure_days_supply)
summary(df$exposure_max_daily_dose_mme)

df_7_day <- fst::read_fst(paste0(drv_root ,"msk_cohort_clean_imputed_7_day_gap.fst")) |>
  as.data.table()

summary(df_7_day$exposure_days_supply)
summary(df_7_day$exposure_max_daily_dose_mme)

mme_30_day <- ggplot(df, aes(x = exposure_max_daily_dose_mme)) +
  geom_histogram(binwidth = 0.5, fill = "black", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Maximum Daily MME", x = "MME", y = "") + 
  scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 50000, by = 10000))

days_30_day <- ggplot(df, aes(x = exposure_days_supply)) +
  geom_histogram(binwidth = 0.5, fill = "black", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Days' Supply", x = "Days", y = "") + 
  scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 50000, by = 10000))

# mme_7_day <- ggplot(df_7_day, aes(x = exposure_max_daily_dose_mme)) +
#   geom_histogram(binwidth = 0.5, fill = "black", color = "black") +
#   theme_minimal() +
#   labs(title = "", x = "MME", y = "") +
#   labs(title = "Histogram of Maximum Daily MME", x = "MME", y = "") + 
#   scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 50000, by = 10000)) 
# 
# days_7_day <- ggplot(df_7_day, aes(x = exposure_days_supply)) +
#   geom_histogram(binwidth = 0.5, fill = "black", color = "black") +
#   theme_minimal() +
#   labs(title = "", x = "Days' Supply", y = "") + 
#   scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 50000, by = 10000)) 

exposures_plot <- mme_30_day + days_30_day 

ggsave(plot = exposures_plot, filename = here::here("figures/exposures_plot.pdf"),
       width = 12, height = 9, dpi = 300, units = "in")
