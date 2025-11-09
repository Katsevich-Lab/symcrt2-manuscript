# Plotting Utilities for symcrt2 Figures
# Contains shared functions used across multiple plotting scripts

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

#' Build vary dataframe for parameter grid reorganization
#'
#' @param parameter_grid The full parameter grid
#' @param vary_parameters Character vector of parameters that vary
#' @param default_row The default parameter configuration row
#' @return Data frame with vary_type column indicating which parameter varies
build_vary_dataframe <- function(parameter_grid, vary_parameters, default_row) {
  vary_defaults <- as.vector(default_row[vary_parameters])
  vary <- data.frame()

  for (i in 1:length(vary_parameters)) {
    vary_dummy <- vary_parameters[i]
    vary_df <- parameter_grid %>% filter(!!as.symbol(vary_dummy) != vary_defaults[[i]])
    vary_df <- dplyr::bind_rows(default_row, vary_df)
    vary_df$vary_type <- vary_dummy
    vary <- dplyr::bind_rows(vary, vary_df)
  }

  return(vary)
}

#' Map method names to display names for Gaussian (stat) simulations
#'
#' @param metrics Data frame with method column
#' @return Data frame with mapped method names
map_method_names_gaussian <- function(metrics) {
  metrics %>% mutate(method = case_when(
    method == "oat_pcm_bam_bam_cs_1" ~ "PCM",
    method == "drvs_gcm_oracle_oracle_1" ~ "oracle GCM",
    method == "hrt_banded_precision_bam_cs_1" ~ "HRT",
    method == "split_pcm_banded_precision_bam_cs_2" ~ "tPCM",
    TRUE ~ method
  ))
}

#' Map method names for proportion comparison (Gaussian)
#'
#' @param metrics Data frame with method column
#' @return Data frame with mapped method names
map_method_names_proportion_gaussian <- function(metrics) {
  metrics %>% mutate(method = case_when(
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
}

#' Map method names to display names for HMM simulations
#'
#' @param metrics Data frame with method column
#' @return Data frame with mapped method names
map_method_names_hmm <- function(metrics) {
  metrics %>% mutate(method = case_when(
    method == "oat_pcm" ~ "PCM",
    method == "gcm" ~ "oracle GCM",
    method == "hrt" ~ "HRT",
    method == "split_pcm" ~ "tPCM",
    method == "knockoff" ~ "Knockoffs",
    TRUE ~ method
  ))
}

#' Map method names for proportion comparison (HMM)
#'
#' @param metrics Data frame with method column
#' @return Data frame with mapped method names
map_method_names_proportion_hmm <- function(metrics) {
  metrics %>% mutate(method = case_when(
    method == "oat_pcm_rf_rf_rf_cs_1" ~ "PCM 0.3",
    method == "oat_pcm_rf_rf_rf_cs_2" ~ "PCM 0.4",
    method == "oat_pcm_rf_rf_rf_cs_3" ~ "PCM 0.5",
    method == "split_pcm_rf_rf_rf_cs_1" ~ "tPCM 0.3",
    method == "split_pcm_rf_rf_rf_cs_2" ~ "tPCM 0.4",
    method == "split_pcm_rf_rf_rf_cs_3" ~ "tPCM 0.5",
    method == "split_pcm_rf_rf_rf_cs_4" ~ "tPCM 0.6",
    method == "split_pcm_rf_rf_rf_cs_5" ~ "tPCM 0.7",
    TRUE ~ method
  ))
}

#' Add vary column and errorbar widths to plotting data
#'
#' @param plotting Data frame with vary_type column
#' @param vary_parameters Character vector of parameters that vary
#' @param errorbar_widths Named list of errorbar widths per parameter
#' @return Data frame with vary column and errorbar_width column
add_vary_column <- function(plotting, vary_parameters, errorbar_widths) {
  plotting$vary <- ""
  for (m in 1:nrow(plotting)) {
    plotting$vary[m] <- plotting[m, plotting$vary_type[m]]
  }

  plotting <- plotting %>%
    mutate(errorbar_width = case_when(
      vary_type == names(errorbar_widths)[1] ~ errorbar_widths[[1]],
      vary_type == names(errorbar_widths)[2] ~ errorbar_widths[[2]],
      vary_type == names(errorbar_widths)[3] ~ errorbar_widths[[3]],
      vary_type == names(errorbar_widths)[4] ~ errorbar_widths[[4]],
      vary_type == names(errorbar_widths)[5] ~ errorbar_widths[[5]]
    ))

  return(plotting)
}

#' Create a single errorbar plot for one varying parameter
#'
#' @param plotting_data Data filtered for specific metric and vary_type
#' @param x_breaks Vector of x-axis breaks
#' @param y_breaks Vector of y-axis breaks
#' @param x_label X-axis label
#' @param alpha_level Optional horizontal line for alpha level (NULL = no line)
#' @param show_legend Logical, whether to show legend
#' @return ggplot object
create_errorbar_plot <- function(plotting_data, x_breaks, y_breaks, x_label,
                                  alpha_level = NULL, show_legend = FALSE) {
  p <- plotting_data %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = x_label, color = NULL) +
    theme_bw() +
    theme(axis.title.y = element_blank())

  if (!is.null(alpha_level)) {
    p <- p + geom_hline(yintercept = alpha_level, linetype = "dashed")
  }

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }

  return(p)
}

#' Create multi-panel grid plot with shared legend and y-axis label
#'
#' @param plot_list List of 5 ggplot objects (for 5 varying parameters)
#' @param y_label Y-axis label (e.g., "FWER", "Power", "FDP")
#' @return Combined plot_grid object
create_grid_plot <- function(plot_list, y_label) {
  # Extract legend from the last plot (assumes it has legend)
  legend <- get_legend(plot_list[[5]])

  # Remove legend from last plot
  plot_list[[5]] <- plot_list[[5]] + theme(legend.position = "none")

  # Create top and bottom rows
  top <- plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], ncol = 3)
  bottom <- plot_grid(plot_list[[4]], plot_list[[5]], legend, ncol = 3)

  # Combine rows
  combined <- plot_grid(top, bottom, ncol = 1, align = 'v')

  # Add y-axis label
  y_label_plot <- ggdraw() +
    draw_label(y_label, x = 0.15, y = 0.5, vjust = 0.5, hjust = 0.5,
               angle = 90, fontface = 'bold', size = 15)

  final_plot <- plot_grid(y_label_plot, combined, ncol = 2, rel_widths = c(0.1, 1))

  return(final_plot)
}

#' Load simulation results
#'
#' @param sim_name Simulation name
#' @param data_dir Base data directory
#' @return Results list
load_sim_results <- function(sim_name, data_dir) {
  results_path <- paste0(data_dir, "/private/results/", sim_name, "/", sim_name, "_results.rds")
  readRDS(results_path)
}

#' Load simulation specification
#'
#' @param sim_name Simulation name
#' @param spec_dir Base specification directory (default: "code/sim_spec")
#' @return Simulatr spec object
load_sim_spec <- function(sim_name, spec_dir = "code/sim_spec") {
  spec_path <- paste0(spec_dir, "/sim_spec_", sim_name, ".rds")
  readRDS(spec_path)
}
