library(tidyverse)
library(lmtp)
library(ggpubr)
library(gridExtra)
library(grid)
library(scales)
library(Cairo)

tidy.lmtp_survival <- function(x, ...) {
  out <- do.call("rbind", lapply(x, tidy))
  out$t <- 1:length(x)
  out[, c(ncol(out), 1:ncol(out) - 1)]
}

isotonic_projection <- function(x, alpha = 0.05) {
  cv <- abs(qnorm(p = alpha / 2))
  estim <- tidy.lmtp_survival(x)
  iso_fit <- isotone::gpava(1:length(x), estim$estimate)
  for (i in seq_along(x)) {
    x[[i]]$theta <- iso_fit$x[i]
    x[[i]]$low <- (x[[i]]$theta - (qnorm(0.975) * x[[i]]$standard_error))
    x[[i]]$high <- (x[[i]]$theta + (qnorm(0.975) * x[[i]]$standard_error))
  }
  x
}

read_results <- function(t, shift, conditional, path){
  data <- readRDS((paste0("/mnt/general-data/disability/everything-local-lmtp/", path, "/", shift, 
                          if(is.null(shift)){
                            NULL} else{
                              "_"},
                          conditional, "_time_", t, "_7_day_gap.rds")))
}

combined_results_list <- list()
contrast_results_list <- list()
density_ratios <- list()

for(i in c("subset_B1", "subset_B2", "subset_B3", "subset_B4", "subset_B5", "subset_B6", "subset_B7", "subset_B8", 
           "subset_B_under_20", "subset_B_not_risky_days", "subset_B_days_7_dose_under_20"))
{

  results_shift <- list()
  density_shift <- list()
  combined_results_df <- data.frame()
  
  shift <- case_when(i == "subset_B1" ~ "d1",
                     i == "subset_B2" ~ "d2",
                     i == "subset_B3" ~ "d3",
                     i == "subset_B4" ~ "d1",
                     i == "subset_B5" ~ "d3",
                     i == "subset_B6" ~ "d2",
                     i == "subset_B7" ~ "d3",
                     i == "subset_B8" ~ "d3",
                     i == "subset_B_under_20" ~ "d1",
                     i == "subset_B_not_risky_days" ~ "d2",
                     i == "subset_B_days_7_dose_under_20" ~ "d3"
  )
  
for (z in c("obs", shift))
             
{
  results_t <- list()
  for (j in 1:5)
  {
    results_t[[j]] <- read_results(as.character(j), as.character(z), as.character(i), path = "results_final_r1") 
      
    if(j > 1)
    {
    results_t[[j]]$theta <- 1 - results_t[[j]]$theta
    old_low <- results_t[[j]]$low
    results_t[[j]]$low <- 1 - results_t[[j]]$high
    results_t[[j]]$high <- 1 - old_low
    }
  }
  
  # density ratio summary
  #density_shift[[z]] <- summary(t(apply(as.matrix(results_t[[5]]$density_ratios),1,cumprod)))
  density_shift[[z]] <- summary(as.matrix(results_t[[5]]$density_ratios))

  results_shift[[z]] <- isotonic_projection(results_t)
}

tidied_results <- map(results_shift, ~ map_dfr(.x, tidy))

dfobs <- tidied_results[[1]] |>
  mutate(shift = "obs",
         t = row_number())

dfshift <- tidied_results[[2]]|>
  mutate(shift = shift, 
         t = row_number())

combined_results_df <- dfobs |>
  merge(dfshift, all = TRUE) |>
  mutate(shift = factor(shift, levels = c("obs", "d1", "d2", "d3"))) |>
  distinct() |>
  arrange(t, shift)

contrast_shift_obs <- map2(results_shift[[2]], results_shift[[1]], ~lmtp_contrast(.x, ref = .y))


combined_vals_contrast_shift_obs<- map_dfr(contrast_shift_obs, ~ {
  data.frame(vals = .x$vals)  
}) |>
  mutate(t = row_number())

contrasts_df <- combined_vals_contrast_shift_obs |>
  mutate(contrast = "shift v. obs") |>
  arrange(t, contrast)

colnames(contrasts_df) <- gsub("^vals\\.", "", colnames(contrasts_df))

combined_results_list[[i]] <- combined_results_df
contrast_results_list[[i]] <- contrasts_df

density_ratios[[i]] <- density_shift
}

# reading ATE

ate_results_list <- list()
for( d in c("obs", "d1", "d2", "d3"))
{
  ate_results <- list()
  for (j in 1:5)
  {
    ate_results[[j]] <- readRDS((paste0("/mnt/general-data/disability/everything-local-lmtp/results_final_r1/", d, "_cohort_time_", j, "_7_day_gap.rds")))
    if (j > 1)
    {
    ate_results[[j]]$theta <- 1 - ate_results[[j]]$theta
    old_low <- ate_results[[j]]$low
    ate_results[[j]]$low <- 1 - ate_results[[j]]$high
    ate_results[[j]]$high <- 1 - old_low
    }
  }
  

  ate_results_list[[d]] <- isotonic_projection(ate_results)
}
tidied_results <- map(ate_results_list, ~ map_dfr(.x, tidy))

dfobs <- tidied_results[[1]] |>
  mutate(shift = "obs",
         t = row_number())

dfd1 <- tidied_results[[2]]|>
  mutate(shift = "d1", 
         t = row_number())

dfd2<- tidied_results[[3]]|>
  mutate(shift = "d2", 
         t = row_number())

dfd3 <- tidied_results[[4]]|>
  mutate(shift = "d3", 
         t = row_number())

combined_results_df_ATE <- dfobs |>
  merge(dfd1, all = TRUE) |>
  merge(dfd2, all = TRUE) |>
  merge(dfd3, all = TRUE) |>
  mutate(shift = factor(shift, levels = c("obs", "d1", "d2", "d3"))) |>
  arrange(t, shift) |>
  mutate(shift = case_when(shift == "obs" ~ "Observed (Cohort)",
                           shift == "d1" ~ "d1 (Cohort)",
                           shift == "d2" ~ "d2 (Cohort)",
                           shift == "d3" ~ "d3 (Cohort)"
                           )
  )


contrast_d1_obs <- map2(ate_results_list[[2]], ate_results_list[[1]], ~lmtp_contrast(.x, ref = .y))
contrast_d2_obs <- map2(ate_results_list[[3]], ate_results_list[[1]], ~lmtp_contrast(.x, ref = .y))
contrast_d3_obs <- map2(ate_results_list[[4]], ate_results_list[[1]], ~lmtp_contrast(.x, ref = .y))


combined_vals_contrast_d1_obs<- map_dfr(contrast_d1_obs, ~ {
  data.frame(vals = .x$vals)  
}) |>
  mutate(t = row_number())

combined_vals_contrast_d2_obs<- map_dfr(contrast_d2_obs, ~ {
  data.frame(vals = .x$vals)  
}) |>
  mutate(t = row_number())

combined_vals_contrast_d3_obs<- map_dfr(contrast_d3_obs, ~ {
  data.frame(vals = .x$vals)  
}) |>
  mutate(t = row_number())

contrasts_df_ATE <- combined_vals_contrast_d1_obs |>
  mutate(contrast = "d1 v. obs") |>
  merge(combined_vals_contrast_d2_obs |> mutate(contrast = "d2 v. obs"), all = TRUE) |>
  merge(combined_vals_contrast_d3_obs |> mutate(contrast = "d3 v. obs"), all = TRUE) |>
  mutate(contrast = factor(contrast, levels = c("d1 v. obs", "d2 v. obs", "d3 v. obs"))) |>
  arrange(t, contrast)

colnames(contrasts_df_ATE) <- gsub("^vals\\.", "", colnames(contrasts_df_ATE))


# Plots
base_plot <-ggplot() +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) + 
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'dashed'),
        panel.grid.minor = element_blank())

d1_data <- combined_results_list$subset_B1 |> mutate(shift = case_when(shift == "d1" ~ "d1 (MME \u2265 50)",
                                                                       shift == "obs" ~ "Observed (MME \u2265 50)")) |>
  merge(combined_results_list$subset_B4 |> mutate(shift = case_when(shift == "d1" ~ "d1 (MME \u2265 90)",
                                                                    shift == "obs" ~ "Observed (MME \u2265 90)")), all = TRUE) |>
  merge(combined_results_list$subset_B_under_20 |> mutate(shift = case_when(shift == "d1" ~ "d1 (MME \u2264 20)",
                                                                            shift == "obs" ~ "Observed (MME \u2264 20)")), all = TRUE) |>
  merge(combined_results_df_ATE |> filter(shift == "Observed (Cohort)" | shift == "d1 (Cohort)"), all = TRUE) |>
  mutate(shift = factor(shift, levels = c("Observed (MME \u2265 90)", "d1 (MME \u2265 90)", "Observed (MME \u2265 50)", "d1 (MME \u2265 50)", "Observed (Cohort)", "d1 (Cohort)", "Observed (MME \u2264 20)", "d1 (MME \u2264 20)")))

results_plot_d1 <- base_plot +
  geom_point(data = d1_data, aes(x = factor(t), y = estimate, color = shift, group = shift, shape = shift),
             position = position_dodge(width = 0.75)) +
  geom_errorbar(data = d1_data, 
                aes(x = factor(t), color = shift, group = shift,
                    ymin = conf.low, ymax = conf.high), width = 0.1, 
                position = position_dodge(width = 0.75)) + 
  labs(x = "", y = "Incidence of OUD", title = "d1. Reducing MME by 20%") +
  labs(color = "Treatment Regime (Subgroup)",
       shape = "Treatment Regime (Subgroup)") + 
  theme_minimal() + 
  scale_color_manual(values = c("#2E8B57", "#2E8B57", "#000000", "#000000", "#6495ED", "#6495ED", "#FF7F00","#FF7F00")) +
  scale_shape_manual(values = c(9, 18, 1, 19, 12, 15, 13, 8)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0, 0.1),
                     labels = label_number(accuracy = 0.0001)) +
  theme(
    plot.title = element_text(hjust = 0, size = 14),
    legend.position =  c(0.3, 0.75),
    legend.key.height = unit(0.8, "lines"),
    legend.key.width = unit(5, "lines"),
    legend.text = element_text(size = 9, margin = margin(r = 4)),
    legend.title = element_text(face = "bold", size = 9),
    legend.background = element_rect(fill = "white", color = "black", size = 0.25), 
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
    legend.spacing.y = unit(0.1, "cm"),
    plot.margin = unit(c(5.5, 5.5, 5.5, 9.5), "pt")
  )

d1_data_contrast <- contrast_results_list$subset_B1 |> mutate(contrast = "d1 v. Observed (MME \u2265 50)") |>
  merge(contrast_results_list$subset_B4 |> mutate(contrast = "d1 v. Observed (MME \u2265 90)"), all = TRUE) |>
  merge(contrast_results_list$subset_B_under_20 |> mutate(contrast = "d1 v. Observed (MME \u2264 20)"), all = TRUE) |>
  merge(contrasts_df_ATE |> filter(contrast == "d1 v. obs") |> mutate(contrast = "d1 v. Observed (Cohort)"), all = TRUE) |>
  mutate(contrast = factor(contrast, levels = c("d1 v. Observed (MME \u2265 90)", "d1 v. Observed (MME \u2265 50)", "d1 v. Observed (Cohort)", "d1 v. Observed (MME \u2264 20)")))

contrast_plot_d1 <- ggplot(data = d1_data_contrast, aes(x = factor(t), y = theta, color = contrast, group = contrast, shape = contrast)) +
  geom_point(position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position = position_dodge(width = 0.75)) +
  labs(x = "Time Period (3 month intervals)", y = "Risk Difference (v. Observed)", title = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("#2E8B57","#000000", "#6495ED", "#FF7F00")) +
  scale_shape_manual(values = c(18, 19, 15, 8)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) + 
  scale_y_continuous(limits = c(-0.0225, 0.005),
                     labels = label_number(accuracy = 0.0001)) + 
  labs(color = "Regime Contrast") + 
  theme_minimal() +
  theme(legend.position = "none")


plots_d1 <- ggarrange(results_plot_d1, 
                      contrast_plot_d1,
                      labels = c("A", "C"),
                      align = "h",
                      nrow = 2)

d2_data <- combined_results_list$subset_B2 |> mutate(shift = case_when(shift == "d2" ~ "d2 (Days > 7)",
                                                                       shift == "obs" ~ "Observed (Days > 7)")) |>
  merge(combined_results_list$subset_B6 |> mutate(shift = case_when(shift == "d2" ~ "d2 (Days > 30)",
                                                                    shift == "obs" ~ "Observed (Days > 30)")), all = TRUE) |>
  merge(combined_results_list$subset_B_not_risky_days |> mutate(shift = case_when(shift == "d2" ~ "d2 (Days \u2264 7)",
                                                                                  shift == "obs" ~ "Observed (Days \u2264 7)")), all = TRUE) |>
  merge(combined_results_df_ATE |> filter(shift == "Observed (Cohort)" | shift == "d2 (Cohort)"), all = TRUE) |>
  mutate(shift = factor(shift, levels = c("Observed (Days > 30)", "d2 (Days > 30)", 
                                          "Observed (Days > 7)", "d2 (Days > 7)", 
                                          "Observed (Cohort)", "d2 (Cohort)",
                                          "Observed (Days \u2264 7)", "d2 (Days \u2264 7)")))


results_plot_d2 <- base_plot +
  geom_point(data = d2_data, aes(x = factor(t), y = estimate, color = shift, group = shift, shape = shift),
             position = position_dodge(width = 0.75)) +
  geom_errorbar(data = d2_data, 
                aes(x = factor(t), color = shift, group = shift,
                    ymin = conf.low, ymax = conf.high), width = 0.1, 
                position = position_dodge(width = 0.75)) + 
  labs(x = "", y = "Incidence of OUD", title = "d2. Reducing Days' Supply by 20%") +
  labs(color = "Treatment Regime (Subgroup)",
       shape = "Treatment Regime (Subgroup)") + 
  theme_minimal() + 
  scale_color_manual(values = c("#2E8B57", "#2E8B57", "#000000", "#000000", "#6495ED", "#6495ED", "#FF7F00","#FF7F00")) +
  scale_shape_manual(values = c(9, 18, 1, 19, 12, 15, 13, 8)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0, 0.1),
                     labels = label_number(accuracy = 0.0001)) +
  theme(
    plot.title = element_text(hjust = 0, size = 14),
    legend.position =  c(0.3, 0.75),
    legend.key.height = unit(0.8, "lines"),
    legend.key.width = unit(5, "lines"),
    legend.text = element_text(size = 9, margin = margin(r = 4)),
    legend.title = element_text(face = "bold", size = 9),
    legend.background = element_rect(fill = "white", color = "black", size = 0.25), 
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
    legend.spacing.y = unit(0.1, "cm"),
    plot.margin = unit(c(5.5, 5.5, 5.5, 9.5), "pt")
  )

d2_data_contrast <- contrast_results_list$subset_B2 |> mutate(contrast = "d2 v. Observed (Days > 7)") |>
  merge(contrast_results_list$subset_B6 |> mutate(contrast = "d2 v. Observed (Days > 30)"), all = TRUE) |>
  merge(contrast_results_list$subset_B_not_risky_days |> mutate(contrast = "d2 v. Observed (Days \u2264 7)"), all = TRUE) |>
  merge(contrasts_df_ATE |> filter(contrast == "d2 v. obs") |> mutate(contrast = "d2 v. Observed (Cohort)"), all = TRUE) |>
  mutate(contrast = factor(contrast, levels = c("d2 v. Observed (Days > 30)", "d2 v. Observed (Days > 7)",
                                                "d2 v. Observed (Cohort)",
                                                "d2 v. Observed (Days \u2264 7)")))

contrast_plot_d2 <- ggplot(data = d2_data_contrast, aes(x = factor(t), y = theta, color = contrast, group = contrast, shape = contrast)) +
  geom_point(position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position = position_dodge(width = 0.75)) +
  labs(x = "Time Period (3 month intervals)", y = "Risk Difference (v. Observed)", title = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("#2E8B57","#000000", "#6495ED", "#FF7F00")) +
  scale_shape_manual(values = c(18, 19, 15, 8)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) + 
  scale_y_continuous(limits = c(-0.0225, 0.005),
                     labels = label_number(accuracy = 0.0001)) + 
  labs(color = "Regime Contrast") + 
  theme_minimal() +
  theme(legend.position = "none")


plots_d2 <- ggarrange(results_plot_d2, 
                      contrast_plot_d2,
                      labels = c("B", "D"),
                      align = "h",
                      nrow = 2)

d3_data <- combined_results_list$subset_B3 |> mutate(shift = case_when(shift == "d3" ~ "d3 (MME \u2265 50, Days > 7)",
                                                                       shift == "obs" ~ "Observed (MME \u2265 50, Days > 7)")) |>
  merge(combined_results_list$subset_B5 |> mutate(shift = case_when(shift == "d3" ~ "d3 (MME \u2265 90, Days > 7)",
                                                                    shift == "obs" ~ "Observed (MME \u2265 90, Days > 7)")), all = TRUE) |>
  merge(combined_results_list$subset_B7 |> mutate(shift = case_when(shift == "d3" ~ "d3 (MME \u2265 50, Days > 30)",
                                                                    shift == "obs" ~ "Observed (MME \u2265 50, Days > 30)")), all = TRUE) |>
  merge(combined_results_list$subset_B8 |> mutate(shift = case_when(shift == "d3" ~ "d3 (MME \u2265 90, Days > 30)",
                                                                    shift == "obs" ~ "Observed (MME \u2265 90, Days > 30)")), all = TRUE) |>
  merge(combined_results_list$subset_B_days_7_dose_under_20 |> mutate(shift = case_when(shift == "d3" ~ "d3 (MME \u2264 20, Days \u2264 7)",
                                                                                        shift == "obs" ~ "Observed (MME \u2264 20, Days \u2264 7)")), all = TRUE) |>
  merge(combined_results_df_ATE |> filter(shift == "Observed (Cohort)" | shift == "d3 (Cohort)"), all = TRUE) |>
  mutate(shift = factor(shift, levels = c("Observed (MME \u2265 90, Days > 30)", "d3 (MME \u2265 90, Days > 30)", 
                                          "Observed (MME \u2265 50, Days > 30)", "d3 (MME \u2265 50, Days > 30)", 
                                          "Observed (MME \u2265 90, Days > 7)", "d3 (MME \u2265 90, Days > 7)", 
                                          "Observed (MME \u2265 50, Days > 7)", "d3 (MME \u2265 50, Days > 7)", 
                                          "Observed (Cohort)", "d3 (Cohort)",
                                          "Observed (MME \u2264 20, Days \u2264 7)", "d3 (MME \u2264 20, Days \u2264 7)")))


results_plot_d3 <- base_plot +
  geom_point(data = d3_data, aes(x = factor(t), y = estimate, color = shift, group = shift, shape = shift),
             position = position_dodge(width = 0.75)) +
  geom_errorbar(data = d3_data, 
                aes(x = factor(t), color = shift, group = shift,
                    ymin = conf.low, ymax = conf.high), width = 0.1, 
                position = position_dodge(width = 0.75)) + 
  labs(x = "", y = "Incidence of OUD", title = "d3. Reducing MME and Days' Supply by 20%") +
  labs(color = "Treatment Regime (Subgroup)",
       shape = "Treatment Regime (Subgroup)") + 
  theme_minimal() + 
  scale_color_manual(values = c("#DAA520", "#DAA520","#950606", 
                                "#950606", "#2E8B57", "#2E8B57","#000000", "#000000", "#6495ED", "#6495ED", "#FF7F00","#FF7F00")) +
  scale_shape_manual(values = c(2, 17, 10, 
                                3, 9, 18, 1, 19, 12, 15, 13, 8)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0, 0.1300),
                     labels = label_number(accuracy = 0.0001)) +
  theme(
    plot.title = element_text(hjust = 0, size = 14),
    legend.position =  c(0.2, 0.725),
    legend.key.height = unit(0.8, "lines"),
    legend.key.width = unit(5, "lines"),
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 9),
    legend.background = element_rect(fill = "white", color = "black", size = 0.25), 
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
    legend.spacing.y = unit(0.1, "cm"),
    plot.margin = unit(c(5.5, 5.5, 5.5, 9.5), "pt")
  )

d3_data_contrast <- contrast_results_list$subset_B3 |> mutate(contrast = "d3 v. Observed (MME \u2265 50, Days > 7)") |>
  merge(contrast_results_list$subset_B5 |> mutate(contrast = "d3 v. Observed (MME \u2265 90, Days > 7)"), all = TRUE) |>
  merge(contrast_results_list$subset_B7 |> mutate(contrast = "d3 v. Observed MME \u2265 50, Days > 30)"), all = TRUE) |>
  merge(contrast_results_list$subset_B8 |> mutate(contrast = "d3 v. Observed (MME \u2265 90, Days > 30)"), all = TRUE) |>
  merge(contrast_results_list$subset_B_days_7_dose_under_20 |> mutate(contrast = "d3 v. Observed (MME \u2264 20, Days \u2264 7)"), all = TRUE) |>
  merge(contrasts_df_ATE |> filter(contrast == "d3 v. obs") |> mutate(contrast = "d3 v. Observed (Cohort)"), all = TRUE) |>
  mutate(contrast = factor(contrast, levels = c("d3 v. Observed (MME \u2265 90, Days > 30)", "d3 v. Observed MME \u2265 50, Days > 30)", 
                                                "d3 v. Observed (MME \u2265 90, Days > 7)", "d3 v. Observed (MME \u2265 50, Days > 7)", 
                                                "d3 v. Observed (Cohort)",
                                                "d3 v. Observed (MME \u2264 20, Days \u2264 7)")))

contrast_plot_d3 <- ggplot(data = d3_data_contrast, aes(x = factor(t), y = theta, color = contrast, group = contrast, shape = contrast)) +
  geom_point(position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position = position_dodge(width = 0.75)) +
  labs(x = "Time Period (3 month intervals)", y = "Risk Difference (v. Observed)", title = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("#DAA520", "#950606", 
                                "#2E8B57","#000000", "#6495ED", "#FF7F00")) +
  scale_shape_manual(values = c(17, 3, 18, 19, 15, 8)) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) + 
  scale_y_continuous(limits = c(-0.04, 0.0025),
                     labels = label_number(accuracy = 0.0001)) + 
  labs(color = "Regime Contrast") + 
  theme_minimal() +
  theme(legend.position = "none")


plots_d3 <- ggarrange(results_plot_d3, 
                      contrast_plot_d3,
                      labels = c("A", "B"),
                      align = "h",
                      nrow = 2)

primary_plot_1 <- ggarrange(plots_d1, plots_d2,
                          align = "h",
                          font.label = list(size = 10),
                          nrow = 1) + 
  annotation_custom(
    grob = grid.lines(x = c(1/2), y = c(0, 1), gp = gpar(col = "black", lwd = 1.5))
  ) +
  annotation_custom(
    grob = grid.lines(x = c(1/2), y = c(0, 1), gp = gpar(col = "black", lwd = 1.5))
  )

primary_plot_2 <- ggarrange(plots_d3,
                          align = "h",
                          font.label = list(size = 10),
                          nrow = 1)
primary_plot_1

primary_plot_2

ggsave(plot = primary_plot_1, filename = here::here("figures/primary_figure_final_1.pdf"),
       width = 12, height = 9, dpi = 300, units = "in", device = cairo_pdf)

ggsave(plot = primary_plot_2, filename = here::here("figures/primary_figure_final_2.pdf"),
       width = 12, height = 9, dpi = 300, units = "in", device = cairo_pdf)

saveRDS(d1_data, "~/everything-local-lmtp/results_for_figure/d1_data.rds")
saveRDS(d2_data, "~/everything-local-lmtp/results_for_figure/d2_data.rds")
saveRDS(d3_data, "~/everything-local-lmtp/results_for_figure/d3_data.rds")

saveRDS(d1_data_contrast, "~/everything-local-lmtp/results_for_figure/d1_data_contrast.rds")
saveRDS(d2_data_contrast, "~/everything-local-lmtp/results_for_figure/d2_data_contrast.rds")
saveRDS(d3_data_contrast, "~/everything-local-lmtp/results_for_figure/d3_data_contrast.rds")

# adding ATE
contrast_results_list$cohort <- contrasts_df_ATE
combined_results_list$cohort <- combined_results_df_ATE |>
  mutate(shift = case_when(shift == "Observed (Cohort)" ~ "obs",
                           shift == "d1 (Cohort)" ~ "d1",
                           shift == "d2 (Cohort)" ~ "d2",
                           shift == "d3 (Cohort)" ~ "d3"
                           ))


saveRDS(contrast_results_list, here::here("results/primary_contrast_results_list_7_day_gap.rds"))
saveRDS(combined_results_list, here::here("results/primary_combined_results_list_7_day_gap.rds"))


