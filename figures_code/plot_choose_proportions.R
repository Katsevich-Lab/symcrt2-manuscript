# load libraries
# library(tidyverse)
library(kableExtra)
library(dplyr)
library(tidyr)
library(patchwork)
library(cowplot)
library(ggplot2)

# enter the simulation name
split_pcm_name <- "split_pcm_stat_manu"
oat_pcm_name <- "oat_pcm_stat_manu"


# load simulatr specs
split_pcm_simulatr_spec <- readRDS(paste0(
"code/sim_spec/sim_spec_", split_pcm_name, ".rds"))
oat_pcm_simulatr_spec <- readRDS(paste0(
"code/sim_spec/sim_spec_", oat_pcm_name, ".rds"))


parameter_grid <- split_pcm_simulatr_spec@parameter_grid

# load results
split_pcm_results <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/",  
                                    split_pcm_name, "/", split_pcm_name, "_results.rds"))
oat_pcm_results <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/",
                                  oat_pcm_name, "/", oat_pcm_name, "_results.rds"))


# set up plots
parameter_grid <- parameter_grid %>% unnest_wider(X_hyperparams) %>% unnest_wider(y_given_X_hyperparams) 

# Reorganize
vary_parameters <- c("n", "p", "s", "rho", "amplitude")

# default row
default_row <- parameter_grid[3,]
vary_defaults <- as.vector(default_row[vary_parameters])

# build new df
vary <- data.frame()
for (i in 1:length(vary_parameters)) {
  vary_dummy <- vary_parameters[i]
  vary_df <- parameter_grid %>% filter(!!as.symbol(vary_dummy) != vary_defaults[[i]])
  vary_df <- dplyr::bind_rows(default_row,vary_df)
  vary_df$vary_type <- vary_dummy
  vary <- dplyr::bind_rows(vary,vary_df)
}

# split_pcm_banded_precision_bam_cs_2" ~ "tPCM 0.4
split_pcm_metrics <- split_pcm_results$metrics |> filter( 
                                                      metric %in% c("fwe","gb_per_rep","hrs_per_rep","power"))
oat_pcm_metrics <- oat_pcm_results$metrics |> filter(metric %in% c("fwe","gb_per_rep","hrs_per_rep","power"))

# Reorder/reorganize data so that the relevant metrics are plotted in the 
# correct order.
metrics <- bind_rows(split_pcm_metrics,oat_pcm_metrics) %>% unnest_wider(X_hyperparams) %>% unnest_wider(y_given_X_hyperparams) 
metrics <- metrics %>% mutate(method = case_when(
  method == "oat_pcm_bam_bam_cs_1" ~ "PCM 0.3",
  method == "oat_pcm_bam_bam_cs_2" ~ "PCM 0.4",
  method == "oat_pcm_bam_bam_cs_3" ~ "PCM 0.5",
  method == "split_pcm_banded_precision_bam_cs_1" ~ "tPCM 0.3",
  method == "split_pcm_banded_precision_bam_cs_2" ~ "tPCM 0.4",
  method == "split_pcm_banded_precision_bam_cs_3" ~ "tPCM 0.5",
  method == "split_pcm_banded_precision_bam_cs_4" ~ "tPCM 0.6",
  method == "split_pcm_banded_precision_bam_cs_5" ~ "tPCM 0.7",
  TRUE ~ method
))

plotting <- metrics |> filter(metric == "fwe" | metric == 'power' | 
                                metric == 'hrs_per_rep' | metric == 'gb_per_rep') |> 
  mutate(metric = factor(metric, levels = c("fwe", "power", "hrs_per_rep", "gb_per_rep"))) |> 
  left_join(vary, by = vary_parameters)
plotting$vary <- ""
for (m in 1:nrow(plotting)) {
  
  plotting$vary[m] <- plotting[m,plotting$vary_type[m]]
  
}
plotting <- plotting %>%
  mutate(
    errorbar_width = case_when(
      vary_type == "amplitude" ~ 0.025,
      vary_type == "n" ~ 100,
      vary_type == "p" ~ 5,
      vary_type == "rho" ~ 0.075,
      vary_type == "s" ~ 2
    )
  )

PCM_plot <- plotting |> filter(!grepl("t", method))
tPCM_plot <- plotting |> filter(grepl("t", method))
plot_list <- list(PCM_plot, tPCM_plot)
names(plot_list) <- c("PCM_choose_proportion", "tPCM_choose_proportion")
# FWE plots for tPCM
for (name in names(plot_list)) {
  plotting <- plot_list[[name]]
  fwe_amplitude <- plotting |> filter(metric == 'fwe' & vary_type == "amplitude") |> 
    mutate(vary = as.numeric(vary)) |>
    ggplot(aes(x = vary, y = mean, color = method))  +
    geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$amplitude))) +
    scale_y_continuous(breaks = seq(0.02,0.08,0.02)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    geom_hline(data = subset(plotting, metric == "fwe"), 
               aes(yintercept = split_pcm_simulatr_spec@fixed_parameters$alpha), linetype = "dashed") +
    labs(x = "amplitude") + theme_bw() + 
    theme(legend.position = "none", axis.title.y=element_blank())
  fwe_n <- plotting |> filter(metric == 'fwe' & vary_type == "n") |> 
    mutate(vary = as.numeric(vary)) |>
    ggplot(aes(x = vary, y = mean, color = method))  +
    geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$n))) +
    scale_y_continuous(breaks = seq(0.02,0.08,0.02)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    geom_hline(data = subset(plotting, metric == "fwe"), 
               aes(yintercept = split_pcm_simulatr_spec@fixed_parameters$alpha), linetype = "dashed") +
    labs(x = "n") + theme_bw() + 
    theme(legend.position = "none", axis.title.y=element_blank())
  fwe_p <- plotting |> filter(metric == 'fwe' & vary_type == "p") |> 
    mutate(vary = as.numeric(vary)) |>
    ggplot(aes(x = vary, y = mean, color = method))  +
    geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$p))) +
    scale_y_continuous(breaks = seq(0.02,0.08,0.02)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    geom_hline(data = subset(plotting, metric == "fwe"), 
               aes(yintercept = split_pcm_simulatr_spec@fixed_parameters$alpha), linetype = "dashed") +
    labs(x = "p") + theme_bw() + 
    theme(legend.position = "none", axis.title.y=element_blank())
  fwe_rho <- plotting |> filter(metric == 'fwe' & vary_type == "rho") |> 
    mutate(vary = as.numeric(vary)) |>
    ggplot(aes(x = vary, y = mean, color = method))  +
    geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$rho))) + 
    scale_y_continuous(breaks = seq(0.02,0.08,0.02)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    geom_hline(data = subset(plotting, metric == "fwe"), 
               aes(yintercept = split_pcm_simulatr_spec@fixed_parameters$alpha), linetype = "dashed") +
    labs(x = "rho") + theme_bw() + 
    theme(legend.position = "none", axis.title.y=element_blank())
  fwe_s <- plotting |> filter(metric == 'fwe' & vary_type == "s") |> 
    mutate(vary = as.numeric(vary)) |>
    ggplot(aes(x = vary, y = mean, color = method))  +
    geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$s))) +
    scale_y_continuous(breaks = seq(0.02,0.08,0.02)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    geom_hline(data = subset(plotting, metric == "fwe"), 
               aes(yintercept = split_pcm_simulatr_spec@fixed_parameters$alpha), linetype = "dashed") +
    labs(x = "s", color = NULL) + theme_bw() 
  # get legend
  legend <- get_legend(fwe_s)
  # remove legend from plot
  fwe_s <- fwe_s + theme(legend.position = "none", axis.title.y=element_blank())
  # Put everything together
  fwe_top <- plot_grid(fwe_amplitude, fwe_n, fwe_p, ncol = 3)
  fwe_bottom <- plot_grid(fwe_rho, fwe_s, legend, ncol = 3)
  
  # Combine rows and align the plots
  fwe_combined <- plot_grid(fwe_top, fwe_bottom, ncol = 1, align = 'v')
  
  # Draw the common y-axis label
  fwe_y_label_plot <- ggdraw() + draw_label("FWER", x = 0.15, y = 0.5, 
                                            vjust = 0.5, hjust = 0.5,
                                            angle = 90, fontface = 'bold', size = 15)
  
  final_fwe_plot <- plot_grid(fwe_y_label_plot, fwe_combined,
                              ncol = 2, rel_widths = c(0.1, 1))
  
  # Print the final plot
  print(final_fwe_plot)
  ggsave(paste0("manuscript/figures/fwer_",name,".pdf"), plot = final_fwe_plot, width = 6, height = 4.5)
  
  
  # Power plots
  power_amplitude <- plotting |> filter(metric == 'power' & vary_type == "amplitude") |> 
    mutate(vary = as.numeric(vary)) |>
    ggplot(aes(x = vary, y = mean, color = method))  +
    geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$amplitude))) +
    scale_y_continuous(breaks = seq(0.15,0.9,0.15)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = "amplitude") + theme_bw() + 
    theme(legend.position = "none", axis.title.y=element_blank())
  power_n <- plotting |> filter(metric == 'power' & vary_type == "n") |> 
    mutate(vary = as.numeric(vary)) |>
    ggplot(aes(x = vary, y = mean, color = method))  +
    geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$n))) +
    scale_y_continuous(breaks = seq(0.15,0.9,0.15)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = "n") + theme_bw() + 
    theme(legend.position = "none", axis.title.y=element_blank())
  power_p <- plotting |> filter(metric == 'power' & vary_type == "p") |> 
    mutate(vary = as.numeric(vary)) |>
    ggplot(aes(x = vary, y = mean, color = method))  +
    geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$p))) +
    scale_y_continuous(breaks = seq(0.15,0.9,0.15)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = "p") + theme_bw() + 
    theme(legend.position = "none", axis.title.y=element_blank())
  power_rho <- plotting |> filter(metric == 'power' & vary_type == "rho") |> 
    mutate(vary = as.numeric(vary)) |>
    ggplot(aes(x = vary, y = mean, color = method))  +
    geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$rho))) + 
    scale_y_continuous(breaks = seq(0.15,0.9,0.15)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = "rho") + theme_bw() + 
    theme(legend.position = "none", axis.title.y=element_blank())
  power_s <- plotting |> filter(metric == 'power' & vary_type == "s") |> 
    mutate(vary = as.numeric(vary)) |>
    ggplot(aes(x = vary, y = mean, color = method))  +
    geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$s))) +
    scale_y_continuous(breaks = seq(0.15,0.9,0.15)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = "s", color = NULL) + theme_bw() 
  # get legend
  legend <- get_legend(power_s)
  # remove legend from plot
  power_s <- power_s + theme(legend.position = "none", axis.title.y=element_blank())
  # Put everything together
  power_top <- plot_grid(power_amplitude, power_n, power_p, ncol = 3)
  power_bottom <- plot_grid(power_rho, power_s, legend, ncol = 3)
  
  # Combine rows and align the plots
  power_combined <- plot_grid(power_top, power_bottom, ncol = 1, align = 'v')
  
  # Draw the common y-axis label
  power_y_label_plot <- ggdraw() + draw_label("Power", x = 0.15, y = 0.5, 
                                              vjust = 0.5, hjust = 0.5,
                                              angle = 90, fontface = 'bold', size = 15)
  
  final_power_plot <- plot_grid(power_y_label_plot, power_combined,
                                ncol = 2, rel_widths = c(0.1, 1))
  
  # Print the final plot
  print(final_power_plot)
  ggsave(paste0("manuscript/figures/power_",name,".pdf"), plot = final_power_plot, width = 6, height = 4.5)
}





