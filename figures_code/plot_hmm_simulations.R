# Plot HMM Interacted Simulation Results
# Produces 7 figures:
# - figure_1_hmm_interacted_reduced.pdf (Fig 3)
# - fdp_hmm_interacted_fdr.pdf (S1)
# - power_hmm_interacted_fdr.pdf (Fig 4)
# - fdp_tPCM_choose_proportion_rf_hmm.pdf (S2)
# - fdp_PCM_choose_proportion_rf_hmm.pdf (S3)
# - power_tPCM_choose_proportion_rf_hmm.pdf (S4)
# - power_PCM_choose_proportion_rf_hmm.pdf (S5)

# Load libraries
library(purrr)
library(stringr)
library(kableExtra)
library(dplyr)
library(tidyr)
library(patchwork)
library(cowplot)
library(ggplot2)
library(scales)

# Load utility functions
source("figures_code/plotting_utils.R")

# Output directory
output_dir <- "manuscript/figures"

# Alpha level for HMM simulations
alpha <- 0.1

# Simulation name
sim_name <- "hmm_interacted_fdr"

# Get data directory
data_dir <- .get_config_path("LOCAL_SYMCRT2_DATA_DIR")

# ============================================================================
# SECTION 1: Figure 1 - Bar plots for n=3000
# ============================================================================

cat("Creating Figure 1 (HMM interacted reduced)...\n")

method_names_fig1 <- c("split_pcm", "oat_pcm", "hrt", "knockoff", "gcm")

# Define helper function to load metrics
load_metrics_fig1 <- function(method_name) {
  results_path <- paste0(data_dir, "/private/results/", sim_name, "/",
                         method_name, "/", method_name, "_results.rds")
  results <- readRDS(results_path)
  results$metrics %>%
    filter(metric %in% c("power", "hrs_per_rep")) %>%
    mutate(method = method_name)
}

combined_metrics_fig1 <- purrr::map_dfr(method_names_fig1, load_metrics_fig1)

combined_metrics_fig1 <- combined_metrics_fig1 %>%
  unnest_wider(X_hyperparams) %>%
  filter(n == 3000) %>%
  mutate(
    method = case_when(
      method == "split_pcm" ~ "tPCM",
      method == "gcm" ~ "GCM",
      method == "hrt" ~ "HRT",
      method == "oat_pcm" ~ "PCM",
      method == "knockoff" ~ "Knockoffs",
      TRUE ~ method
    ),
    mean = ifelse(metric == "hrs_per_rep", mean * 3600, mean), # hours to seconds
    metric = ifelse(metric == "hrs_per_rep", "sec_per_rep", "power")
  )

# Order methods by computational cost
method_order_fig1 <- combined_metrics_fig1 %>%
  filter(metric == "sec_per_rep") %>%
  arrange(mean) %>%
  pull(method)

combined_metrics_fig1 <- combined_metrics_fig1 %>%
  mutate(method = factor(method, levels = method_order_fig1))

# Create power plot
power_plot <- combined_metrics_fig1 %>%
  filter(metric == "power") %>%
  ggplot(aes(x = method, y = mean, fill = method)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se), width = 0.3) +
  labs(x = NULL, y = "Power", title = "Statistical") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Create computational plot
comp_plot <- combined_metrics_fig1 %>%
  filter(metric == "sec_per_rep") %>%
  ggplot(aes(x = method, y = mean, fill = method)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_log10() +
  labs(x = NULL, y = "Runtime (seconds)", title = "Computational") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Combine plots
final_fig_1 <- plot_grid(comp_plot, power_plot, ncol = 2, align = "v")

ggsave(paste0(output_dir, "/figure_1_hmm_interacted_reduced.pdf"),
       plot = final_fig_1, width = 5, height = 3)
cat("Saved figure_1_hmm_interacted_reduced.pdf\n")

# ============================================================================
# SECTION 2: Main FDP and Power Plots (comparing all 5 methods)
# ============================================================================

cat("\nLoading data for main FDP and power plots...\n")

method_names_main <- c("split_pcm", "oat_pcm", "hrt", "knockoff", "gcm")

# Load simulation spec
split_pcm_spec <- readRDS(paste0("code/sim_spec/", sim_name, "/sim_spec_split_pcm.rds"))
parameter_grid <- split_pcm_spec@parameter_grid %>% unnest_wider(X_hyperparams)

# Build vary dataframe
vary_parameters <- c("n", "p", "s", "stay_prob", "amplitude")
default_row <- parameter_grid[3, ]
vary <- build_vary_dataframe(parameter_grid, vary_parameters, default_row)

# Define helper function to load metrics for main plots
load_metrics_main <- function(method_name) {
  spec_path <- paste0("code/sim_spec/", sim_name, "/sim_spec_", method_name, ".rds")
  results_path <- paste0(data_dir, "/private/results/", sim_name, "/",
                         method_name, "/", method_name, "_results.rds")

  simulatr_spec <- readRDS(spec_path)
  results <- readRDS(results_path)

  metrics <- results$metrics %>%
    filter(metric %in% c("fdp", "gb_per_rep", "hrs_per_rep", "power")) %>%
    mutate(method = method_name)

  return(metrics)
}

combined_metrics_main <- purrr::map_dfr(method_names_main, load_metrics_main)

# Process metrics
metrics_main <- combined_metrics_main %>%
  unnest_wider(X_hyperparams) %>%
  map_method_names_hmm()

# Prepare plotting data
plotting_main <- metrics_main %>%
  left_join(vary, by = vary_parameters)

# Add vary column
plotting_main$vary <- ""
for (m in 1:nrow(plotting_main)) {
  plotting_main$vary[m] <- plotting_main[m, plotting_main$vary_type[m]]
}

# Add errorbar widths
errorbar_widths_hmm <- list(amplitude = 0.05, n = 125, p = 5, stay_prob = 0.0375, s = 2)
plotting_main <- plotting_main %>%
  mutate(errorbar_width = case_when(
    vary_type == "amplitude" ~ 0.05,
    vary_type == "n" ~ 125,
    vary_type == "p" ~ 5,
    vary_type == "stay_prob" ~ 0.0375,
    vary_type == "s" ~ 2
  ))

# -------------------------------------------------------------------
# Plot 1: FDP
# -------------------------------------------------------------------

cat("Creating FDP plot...\n")

fdp_data <- plotting_main %>% filter(metric == 'fdp')

fdp_amplitude <- fdp_data %>%
  filter(vary_type == "amplitude") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = mean, color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$amplitude)),
                     labels = label_number(drop0trailing = TRUE)) +
  scale_y_continuous(breaks = seq(0.02, 0.12, 0.02)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  geom_hline(yintercept = alpha, linetype = "dashed") +
  labs(x = "amplitude") +
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank())

fdp_n <- fdp_data %>%
  filter(vary_type == "n") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = mean, color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$n))) +
  scale_y_continuous(breaks = seq(0.02, 0.12, 0.02)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  geom_hline(yintercept = alpha, linetype = "dashed") +
  labs(x = "n") +
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank())

fdp_p <- fdp_data %>%
  filter(vary_type == "p") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = mean, color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$p))) +
  scale_y_continuous(breaks = seq(0.02, 0.12, 0.02)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  geom_hline(yintercept = alpha, linetype = "dashed") +
  labs(x = "p") +
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank())

fdp_stay_prob <- fdp_data %>%
  filter(vary_type == "stay_prob") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = mean, color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$stay_prob)),
                     labels = label_number(drop0trailing = TRUE)) +
  scale_y_continuous(breaks = seq(0.02, 0.12, 0.02)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  geom_hline(yintercept = alpha, linetype = "dashed") +
  labs(x = "stay_prob") +
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank())

fdp_s <- fdp_data %>%
  filter(vary_type == "s") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = mean, color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$s))) +
  scale_y_continuous(breaks = seq(0.02, 0.12, 0.02)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  geom_hline(yintercept = alpha, linetype = "dashed") +
  labs(x = "s", color = NULL) +
  theme_bw()

final_fdp_plot <- create_grid_plot(
  list(fdp_amplitude, fdp_n, fdp_p, fdp_stay_prob, fdp_s),
  "FDP"
)

ggsave(paste0(output_dir, "/fdp_hmm_interacted_fdr.pdf"),
       plot = final_fdp_plot, width = 6, height = 4.5)
cat("Saved fdp_hmm_interacted_fdr.pdf\n")

# -------------------------------------------------------------------
# Plot 2: Power
# -------------------------------------------------------------------

cat("Creating Power plot...\n")

power_data <- plotting_main %>% filter(metric == 'power')

power_amplitude <- power_data %>%
  filter(vary_type == "amplitude") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = mean, color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$amplitude)),
                     labels = label_number(drop0trailing = TRUE)) +
  scale_y_continuous(breaks = seq(0.3, 0.95, 0.1)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  labs(x = "amplitude") +
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank())

power_n <- power_data %>%
  filter(vary_type == "n") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = mean, color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$n))) +
  scale_y_continuous(breaks = seq(0.3, 0.95, 0.1)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  labs(x = "n") +
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank())

power_p <- power_data %>%
  filter(vary_type == "p") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = mean, color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$p))) +
  scale_y_continuous(breaks = seq(0.3, 0.95, 0.1)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  labs(x = "p") +
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank())

power_stay_prob <- power_data %>%
  filter(vary_type == "stay_prob") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = mean, color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$stay_prob)),
                     labels = label_number(drop0trailing = TRUE)) +
  scale_y_continuous(breaks = seq(0.3, 0.95, 0.1)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  labs(x = "stay_prob") +
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank())

power_s <- power_data %>%
  filter(vary_type == "s") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = mean, color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$s))) +
  scale_y_continuous(breaks = seq(0.3, 0.95, 0.1)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  labs(x = "s", color = NULL) +
  theme_bw()

final_power_plot <- create_grid_plot(
  list(power_amplitude, power_n, power_p, power_stay_prob, power_s),
  "Power"
)

ggsave(paste0(output_dir, "/power_hmm_interacted_fdr.pdf"),
       plot = final_power_plot, width = 6.5, height = 4)
cat("Saved power_hmm_interacted_fdr.pdf\n")

# ============================================================================
# SECTION 3: Training Proportion Comparison Plots
# ============================================================================

cat("\nLoading data for training proportion comparison...\n")

method_names_prop <- c("split_pcm_stat_manu", "split_pcm_proportions",
                       "oat_pcm_stat_manu", "oat_pcm_proportions")

# Load simulation spec
split_pcm_prop_spec <- readRDS(paste0("code/sim_spec/", sim_name, "/sim_spec_split_pcm_stat_manu.rds"))
parameter_grid_prop <- split_pcm_prop_spec@parameter_grid %>% unnest_wider(X_hyperparams)

# Build vary dataframe
default_row_prop <- parameter_grid_prop[3, ]
vary_prop <- build_vary_dataframe(parameter_grid_prop, vary_parameters, default_row_prop)

# Define helper function to load metrics
load_metrics_prop <- function(method_name) {
  spec_path <- paste0("code/sim_spec/", sim_name, "/sim_spec_", method_name, ".rds")
  results_path <- paste0(data_dir, "/private/results/", sim_name, "/",
                         method_name, "/", method_name, "_results.rds")

  simulatr_spec <- readRDS(spec_path)
  results <- readRDS(results_path)

  metrics <- results$metrics %>%
    filter(metric %in% c("fdp", "gb_per_rep", "hrs_per_rep", "power")) %>%
    mutate(old_method = method, method = method_name)

  return(metrics)
}

combined_metrics_prop <- purrr::map_dfr(method_names_prop, load_metrics_prop)

# Process metrics - map to display names using both method and old_method
metrics_prop <- combined_metrics_prop %>%
  unnest_wider(X_hyperparams) %>%
  mutate(method = case_when(
    method == "split_pcm_stat_manu" & old_method == "split_pcm_hmm_fastPhase_ranger_1" ~ "tPCM 0.4",
    method == "split_pcm_proportions" & old_method == "split_pcm_hmm_fastPhase_ranger_1" ~ "tPCM 0.3",
    method == "split_pcm_proportions" & old_method == "split_pcm_hmm_fastPhase_ranger_2" ~ "tPCM 0.35",
    method == "split_pcm_proportions" & old_method == "split_pcm_hmm_fastPhase_ranger_3" ~ "tPCM 0.45",
    method == "split_pcm_proportions" & old_method == "split_pcm_hmm_fastPhase_ranger_4" ~ "tPCM 0.5",
    method == "oat_pcm_stat_manu" & old_method == "oat_pcm_ranger_categorical_ranger_1" ~ "PCM 0.4",
    method == "oat_pcm_proportions" & old_method == "oat_pcm_ranger_categorical_ranger_1" ~ "PCM 0.45",
    method == "oat_pcm_proportions" & old_method == "oat_pcm_ranger_categorical_ranger_2" ~ "PCM 0.5",
    TRUE ~ method
  ))

# Prepare plotting data
plotting_prop <- metrics_prop %>%
  left_join(vary_prop, by = vary_parameters)

# Add vary column
plotting_prop$vary <- ""
for (m in 1:nrow(plotting_prop)) {
  plotting_prop$vary[m] <- plotting_prop[m, plotting_prop$vary_type[m]]
}

# Add errorbar widths
plotting_prop <- plotting_prop %>%
  mutate(errorbar_width = case_when(
    vary_type == "amplitude" ~ 0.05,
    vary_type == "n" ~ 125,
    vary_type == "p" ~ 5,
    vary_type == "stay_prob" ~ 0.0375,
    vary_type == "s" ~ 2
  ))

# Split into PCM and tPCM
PCM_plot_data <- plotting_prop %>% filter(!grepl("t", method))
tPCM_plot_data <- plotting_prop %>% filter(grepl("t", method))

plot_list_prop <- list(PCM = PCM_plot_data, tPCM = tPCM_plot_data)

# -------------------------------------------------------------------
# Create FDP and Power plots for both PCM and tPCM
# -------------------------------------------------------------------

for (method_type in c("PCM", "tPCM")) {
  cat(paste0("\nCreating ", method_type, " proportion comparison plots...\n"))

  plotting_data <- plot_list_prop[[method_type]]

  # FDP plots
  fdp_data_prop <- plotting_data %>% filter(metric == 'fdp')

  fdp_amplitude_prop <- fdp_data_prop %>%
    filter(vary_type == "amplitude") %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = sort(unique(parameter_grid_prop$amplitude)),
                       labels = label_number(drop0trailing = TRUE)) +
    scale_y_continuous(breaks = seq(0.02, 0.12, 0.02)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    geom_hline(yintercept = alpha, linetype = "dashed") +
    labs(x = "amplitude") +
    theme_bw() +
    theme(legend.position = "none", axis.title.y = element_blank())

  fdp_n_prop <- fdp_data_prop %>%
    filter(vary_type == "n") %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = sort(unique(parameter_grid_prop$n))) +
    scale_y_continuous(breaks = seq(0.02, 0.12, 0.02)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    geom_hline(yintercept = alpha, linetype = "dashed") +
    labs(x = "n") +
    theme_bw() +
    theme(legend.position = "none", axis.title.y = element_blank())

  fdp_p_prop <- fdp_data_prop %>%
    filter(vary_type == "p") %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = sort(unique(parameter_grid_prop$p))) +
    scale_y_continuous(breaks = seq(0.02, 0.12, 0.02)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    geom_hline(yintercept = alpha, linetype = "dashed") +
    labs(x = "p") +
    theme_bw() +
    theme(legend.position = "none", axis.title.y = element_blank())

  fdp_stay_prob_prop <- fdp_data_prop %>%
    filter(vary_type == "stay_prob") %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = sort(unique(parameter_grid_prop$stay_prob)),
                       labels = label_number(drop0trailing = TRUE)) +
    scale_y_continuous(breaks = seq(0.02, 0.12, 0.02)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    geom_hline(yintercept = alpha, linetype = "dashed") +
    labs(x = "stay_prob") +
    theme_bw() +
    theme(legend.position = "none", axis.title.y = element_blank())

  fdp_s_prop <- fdp_data_prop %>%
    filter(vary_type == "s") %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = sort(unique(parameter_grid_prop$s))) +
    scale_y_continuous(breaks = seq(0.02, 0.12, 0.02)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    geom_hline(yintercept = alpha, linetype = "dashed") +
    labs(x = "s", color = NULL) +
    theme_bw()

  final_fdp_plot_prop <- create_grid_plot(
    list(fdp_amplitude_prop, fdp_n_prop, fdp_p_prop, fdp_stay_prob_prop, fdp_s_prop),
    "FDP"
  )

  ggsave(
    paste0(output_dir, "/fdp_", method_type, "_choose_proportion_rf_hmm.pdf"),
    plot = final_fdp_plot_prop,
    width = 6,
    height = 4.5
  )
  cat(paste0("Saved fdp_", method_type, "_choose_proportion_rf_hmm.pdf\n"))

  # Power plots
  power_data_prop <- plotting_data %>% filter(metric == 'power')

  power_amplitude_prop <- power_data_prop %>%
    filter(vary_type == "amplitude") %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = sort(unique(parameter_grid_prop$amplitude)),
                       labels = label_number(drop0trailing = TRUE)) +
    scale_y_continuous(breaks = seq(0.3, 0.95, 0.1)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = "amplitude") +
    theme_bw() +
    theme(legend.position = "none", axis.title.y = element_blank())

  power_n_prop <- power_data_prop %>%
    filter(vary_type == "n") %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = sort(unique(parameter_grid_prop$n))) +
    scale_y_continuous(breaks = seq(0.3, 0.95, 0.1)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = "n") +
    theme_bw() +
    theme(legend.position = "none", axis.title.y = element_blank())

  power_p_prop <- power_data_prop %>%
    filter(vary_type == "p") %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = sort(unique(parameter_grid_prop$p))) +
    scale_y_continuous(breaks = seq(0.3, 0.95, 0.1)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = "p") +
    theme_bw() +
    theme(legend.position = "none", axis.title.y = element_blank())

  power_stay_prob_prop <- power_data_prop %>%
    filter(vary_type == "stay_prob") %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = sort(unique(parameter_grid_prop$stay_prob)),
                       labels = label_number(drop0trailing = TRUE)) +
    scale_y_continuous(breaks = seq(0.3, 0.95, 0.1)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = "stay_prob") +
    theme_bw() +
    theme(legend.position = "none", axis.title.y = element_blank())

  power_s_prop <- power_data_prop %>%
    filter(vary_type == "s") %>%
    mutate(vary = as.numeric(vary)) %>%
    ggplot(aes(x = vary, y = mean, color = method)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = sort(unique(parameter_grid_prop$s))) +
    scale_y_continuous(breaks = seq(0.3, 0.95, 0.1)) +
    geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
    labs(x = "s", color = NULL) +
    theme_bw()

  final_power_plot_prop <- create_grid_plot(
    list(power_amplitude_prop, power_n_prop, power_p_prop, power_stay_prob_prop, power_s_prop),
    "Power"
  )

  ggsave(
    paste0(output_dir, "/power_", method_type, "_choose_proportion_rf_hmm.pdf"),
    plot = final_power_plot_prop,
    width = 6,
    height = 4.5
  )
  cat(paste0("Saved power_", method_type, "_choose_proportion_rf_hmm.pdf\n"))
}

cat("\n=== HMM simulation plots complete! ===\n")
cat("Generated 7 figures in", output_dir, "\n")
