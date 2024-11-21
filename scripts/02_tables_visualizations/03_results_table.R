library(tidyverse)
library(lmtp)
library(ggpubr)

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

read_results <- function(t, shift, conditional){
  data <- readRDS((paste0("~/results/local_lmtp_results_", shift, "_", conditional, "_time_", t, ".rds")))
}


combined_results_list <- list()
contrast_results_list <- list()

for(i in c("subset_B1", "subset_B2", "subset_B3"))
{
  results_shift <- list()
  combined_results_df <- data.frame()
  for (z in c("obs", "d1", "d2", "d3"))
  {
    results_t <- list()
    for (j in 1:5)
    {
      if(j > 1)
      {
        results_t[[j]] <- read_results(as.character(j), as.character(z), as.character(i)) 
        results_t[[j]]$theta <- 1 - results_t[[j]]$theta
        old_low <- results_t[[j]]$low
        results_t[[j]]$low <- 1 - results_t[[j]]$high
        results_t[[j]]$high <- 1 - old_low
      } else
      {
        results_t[[j]] <- read_results(as.character(j), as.character(z), as.character(i)) 
      }
    }
    
    results_shift[[z]] <- isotonic_projection(results_t)
  }
  
  tidied_results <- map(results_shift, ~ map_dfr(.x, tidy))
  
  dfobs <- tidied_results[[1]] |>
    mutate(shift = "obs",
           t = row_number())
  
  dfd1 <- tidied_results[[2]]|>
    mutate(shift = "d1", 
           t = row_number())
  
  dfd2 <- tidied_results[[3]]|>
    mutate(shift = "d2", 
           t = row_number())
  
  dfd3 <- tidied_results[[4]]|>
    mutate(shift = "d3", 
           t = row_number())
  
  combined_results_df <- dfobs |>
    merge(dfd1, all = TRUE) |>
    merge(dfd2, all = TRUE) |>
    merge(dfd3, all = TRUE) |>
    mutate(shift = factor(shift, levels = c("obs", "d1", "d2", "d3"))) |>
    arrange(t, shift)
  
  contrast_d1_obs <- map2(results_shift[[2]], results_shift[[1]], ~lmtp_contrast(.x, ref = .y))
  contrast_d2_obs <- map2(results_shift[[3]], results_shift[[1]], ~lmtp_contrast(.x, ref = .y))
  contrast_d3_obs <- map2(results_shift[[4]], results_shift[[1]], ~lmtp_contrast(.x, ref = .y))
  
  
  combined_vals_contrast_d1_obs<- map_dfr(contrast_d1_obs, ~ {
    data.frame(vals = .x$vals)  
  }) |>
    mutate(t = row_number())
  
  colnames(combined_vals_contrast_d1_obs) <- gsub("\\vals.", "", colnames(combined_vals_contrast_d1_obs))
  
  combined_vals_contrast_d2_obs<- map_dfr(contrast_d2_obs, ~ {
    data.frame(vals = .x$vals)  
  }) |>
    mutate(t = row_number())
  
  colnames(combined_vals_contrast_d2_obs) <- gsub("\\vals.", "", colnames(combined_vals_contrast_d2_obs))
  
  combined_vals_contrast_d3_obs<- map_dfr(contrast_d3_obs, ~ {
    data.frame(vals = .x$vals)  
  }) |>
    mutate(t = row_number())
  
  colnames(combined_vals_contrast_d3_obs) <- gsub("\\vals.", "", colnames(combined_vals_contrast_d3_obs))
  
  contrasts_df <- combined_vals_contrast_d1_obs |>
    mutate(contrast = "d1 v. obs") |>
    merge(combined_vals_contrast_d2_obs |> mutate(contrast = "d2 v. obs"), all = TRUE) |>
    merge(combined_vals_contrast_d3_obs |> mutate(contrast = "d3 v. obs"), all = TRUE)  |>
    mutate(contrast = factor(contrast, levels = c("d1 v. obs", "d2 v. obs", "d3 v. obs"))) |>
    arrange(t, contrast)
  
  combined_results_list[[i]] <- combined_results_df
  contrast_results_list[[i]] <- contrasts_df
}

tbl_B1 <- combined_results_list$subset_B1 |>
  mutate(subset = "B1") |>
  mutate(estimate = paste0(round(estimate, 4), "(", round(conf.low, 4), ", ", round(conf.high, 4), ")")) |>
  select(subset, estimate, shift, t) |>
  pivot_wider(names_from = t, values_from = estimate, names_glue = "estimate_{t}")

tbl_B2 <- combined_results_list$subset_B2 |>
  mutate(subset = "B2") |>
  mutate(estimate = paste0(round(estimate, 4), "(", round(conf.low, 4), ", ", round(conf.high, 4), ")")) |>
  select(subset, estimate, shift, t) |>
  pivot_wider(names_from = t, values_from = estimate, names_glue = "estimate_{t}")

tble_B3 <- combined_results_list$subset_B3 |>
  mutate(subset = "B3") |>
  mutate(estimate = paste0(round(estimate, 4), "(", round(conf.low, 4), ", ", round(conf.high, 4), ")")) |>
  select(subset, estimate, shift, t) |>
  pivot_wider(names_from = t, values_from = estimate, names_glue = "estimate_{t}")

tbl_B1_contrast <- contrast_results_list$subset_B1 |>
  mutate(subset = "B1") |>
  mutate(theta = paste0(round(theta, 4), "(", round(conf.low, 4), ", ", round(conf.high, 4), ")")) |>
  select(subset, theta, contrast, t) |>
  pivot_wider(names_from = t, values_from = theta, names_glue = "theta_{t}") |>
  mutate(shift = case_when(contrast == "d1 v. obs" ~ "d1",
                           contrast == "d2 v. obs" ~ "d2",
                           contrast == "d3 v. obs" ~ "d3"
  )) |>
  select(-contrast) |>
  mutate(shift = factor(shift, levels = c("d1", "d2", "d3")))


tbl_B2_contrast <- contrast_results_list$subset_B2 |>
  mutate(subset = "B2") |>
  mutate(theta = paste0(round(theta, 4), "(", round(conf.low, 4), ", ", round(conf.high, 4), ")")) |>
  select(subset, theta, contrast, t) |>
  pivot_wider(names_from = t, values_from = theta, names_glue = "theta_{t}") |>
  mutate(shift = case_when(contrast == "d1 v. obs" ~ "d1",
                           contrast == "d2 v. obs" ~ "d2",
                           contrast == "d3 v. obs" ~ "d3"
  )) |>
  select(-contrast) |>
  mutate(shift = factor(shift, levels = c("d1", "d2", "d3")))

tbl_B3_contrast <- contrast_results_list$subset_B3 |>
  mutate(subset = "B3") |>
  mutate(theta = paste0(round(theta, 4), "(", round(conf.low, 4), ", ", round(conf.high, 4), ")")) |>
  select(subset, theta, contrast, t) |>
  pivot_wider(names_from = t, values_from = theta, names_glue = "theta_{t}") |>
  mutate(shift = case_when(contrast == "d1 v. obs" ~ "d1",
                           contrast == "d2 v. obs" ~ "d2",
                           contrast == "d3 v. obs" ~ "d3"
  )) |>
  select(-contrast) |>
  mutate(shift = factor(shift, levels = c("d1", "d2", "d3")))

