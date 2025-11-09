# Plot Gaussian (stat) Simulation Results
# Produces 6 figures:
# - fwer_stat.pdf (S6)
# - power_stat.pdf (S7)
# - computation_stat.pdf (S8)
# - fwer_tPCM_choose_proportion.pdf (S9)
# - fwer_PCM_choose_proportion.pdf (S10)
# - power_tPCM_choose_proportion.pdf (S11)
# - power_PCM_choose_proportion.pdf (S12)

# Load libraries
library(kableExtra)
library(dplyr)
library(tidyr)
library(patchwork)
library(cowplot)
library(ggplot2)

# Load utility functions
source("figures_code/plotting_utils.R")

# Output directory
output_dir <- "manuscript/figures"

# Get data directory
data_dir <- .get_config_path("LOCAL_SYMCRT2_DATA_DIR")

# ============================================================================
# SECTION 1: Main FWER and Power Plots (comparing all 4 methods)
# ============================================================================

cat("Loading data for main FWER and power plots...\n")

# Simulation names
split_pcm_name <- "split_pcm_stat_manu"
oat_pcm_name <- "oat_pcm_stat_manu"
gcm_name <- "gcm_stat_manu"
hrt_name <- "hrt_stat_manu"

# Load simulation specs
split_pcm_spec <- load_sim_spec(split_pcm_name)
oat_pcm_spec <- load_sim_spec(oat_pcm_name)
gcm_spec <- load_sim_spec(gcm_name)
hrt_spec <- load_sim_spec(hrt_name)

# Load results
split_pcm_results <- load_sim_results(split_pcm_name, data_dir)
oat_pcm_results <- load_sim_results(oat_pcm_name, data_dir)
gcm_results <- load_sim_results(gcm_name, data_dir)
hrt_results <- load_sim_results(hrt_name, data_dir)

# Extract parameter grid
parameter_grid <- split_pcm_spec@parameter_grid %>%
  unnest_wider(X_hyperparams) %>%
  unnest_wider(y_given_X_hyperparams)

# Build vary dataframe
vary_parameters <- c("n", "p", "s", "rho", "amplitude")
default_row <- parameter_grid[3, ]
vary <- build_vary_dataframe(parameter_grid, vary_parameters, default_row)

# Extract and combine metrics
split_pcm_metrics <- split_pcm_results$metrics %>%
  filter(method == "split_pcm_banded_precision_bam_cs_2" &
           metric %in% c("fwe", "gb_per_rep", "hrs_per_rep", "power"))

oat_pcm_metrics <- oat_pcm_results$metrics %>%
  filter(method == "oat_pcm_bam_bam_cs_1" &
           metric %in% c("fwe", "gb_per_rep", "hrs_per_rep", "power"))

gcm_metrics <- gcm_results$metrics %>%
  filter(metric %in% c("fwe", "gb_per_rep", "hrs_per_rep", "power"))

hrt_metrics <- hrt_results$metrics %>%
  filter(metric %in% c("fwe", "gb_per_rep", "hrs_per_rep", "power"))

# Combine and process metrics
metrics <- bind_rows(split_pcm_metrics, oat_pcm_metrics, gcm_metrics, hrt_metrics) %>%
  unnest_wider(X_hyperparams) %>%
  unnest_wider(y_given_X_hyperparams) %>%
  map_method_names_gaussian()

# Prepare plotting data
plotting <- metrics %>%
  filter(metric %in% c("fwe", "power", "hrs_per_rep", "gb_per_rep")) %>%
  mutate(metric = factor(metric, levels = c("fwe", "power", "hrs_per_rep", "gb_per_rep"))) %>%
  left_join(vary, by = vary_parameters)

errorbar_widths <- list(amplitude = 0.025, n = 100, p = 5, rho = 0.075, s = 2)
plotting <- add_vary_column(plotting, vary_parameters, errorbar_widths)

# Get alpha level
alpha_level <- split_pcm_spec@fixed_parameters$alpha

# -------------------------------------------------------------------
# Plot 1: FWER
# -------------------------------------------------------------------

cat("Creating FWER plot...\n")

fwe_data <- plotting %>% filter(metric == 'fwe')

fwe_amplitude <- create_errorbar_plot(
  fwe_data %>% filter(vary_type == "amplitude"),
  sort(unique(parameter_grid$amplitude)),
  seq(0.02, 0.08, 0.02),
  "amplitude",
  alpha_level = alpha_level
)

fwe_n <- create_errorbar_plot(
  fwe_data %>% filter(vary_type == "n"),
  sort(unique(parameter_grid$n)),
  seq(0.02, 0.08, 0.02),
  "n",
  alpha_level = alpha_level
)

fwe_p <- create_errorbar_plot(
  fwe_data %>% filter(vary_type == "p"),
  sort(unique(parameter_grid$p)),
  seq(0.02, 0.08, 0.02),
  "p",
  alpha_level = alpha_level
)

fwe_rho <- create_errorbar_plot(
  fwe_data %>% filter(vary_type == "rho"),
  sort(unique(parameter_grid$rho)),
  seq(0.02, 0.08, 0.02),
  "rho",
  alpha_level = alpha_level
)

fwe_s <- create_errorbar_plot(
  fwe_data %>% filter(vary_type == "s"),
  sort(unique(parameter_grid$s)),
  seq(0.02, 0.08, 0.02),
  "s",
  alpha_level = alpha_level,
  show_legend = TRUE
)

final_fwe_plot <- create_grid_plot(
  list(fwe_amplitude, fwe_n, fwe_p, fwe_rho, fwe_s),
  "FWER"
)

ggsave(paste0(output_dir, "/fwer_stat.pdf"), plot = final_fwe_plot, width = 6, height = 4.5)
cat("Saved fwer_stat.pdf\n")

# -------------------------------------------------------------------
# Plot 2: Power
# -------------------------------------------------------------------

cat("Creating Power plot...\n")

power_data <- plotting %>% filter(metric == 'power')

power_amplitude <- create_errorbar_plot(
  power_data %>% filter(vary_type == "amplitude"),
  sort(unique(parameter_grid$amplitude)),
  seq(0.15, 0.9, 0.15),
  "amplitude"
)

power_n <- create_errorbar_plot(
  power_data %>% filter(vary_type == "n"),
  sort(unique(parameter_grid$n)),
  seq(0.15, 0.9, 0.15),
  "n"
)

power_p <- create_errorbar_plot(
  power_data %>% filter(vary_type == "p"),
  sort(unique(parameter_grid$p)),
  seq(0.15, 0.9, 0.15),
  "p"
)

power_rho <- create_errorbar_plot(
  power_data %>% filter(vary_type == "rho"),
  sort(unique(parameter_grid$rho)),
  seq(0.15, 0.9, 0.15),
  "rho"
)

power_s <- create_errorbar_plot(
  power_data %>% filter(vary_type == "s"),
  sort(unique(parameter_grid$s)),
  seq(0.15, 0.9, 0.15),
  "s",
  show_legend = TRUE
)

final_power_plot <- create_grid_plot(
  list(power_amplitude, power_n, power_p, power_rho, power_s),
  "Power"
)

ggsave(paste0(output_dir, "/power_stat.pdf"), plot = final_power_plot, width = 6, height = 4.5)
cat("Saved power_stat.pdf\n")

# -------------------------------------------------------------------
# Plot 3: Computation Time
# -------------------------------------------------------------------

cat("Creating computation time plot...\n")

# Load computational efficiency data (larger p variation)
methods <- c("split_pcm", "drvs_gcm", "hrt", "oat_pcm")

computation_results <- data.frame(
  method = rep(methods, 5),
  p = rep(c(100, 125, 150, 175, 200), each = 4),
  time = rep(0, 20)
)

for (i in 1:nrow(computation_results)) {
  method_name <- computation_results$method[i]
  p_value <- computation_results$p[i]

  result <- readRDS(paste0(
    data_dir, "/private/results/",
    method_name, "_", p_value,
    "/", method_name, "_", p_value, "_results.rds"
  ))
  computation_results$time[i] <- unlist(result$method_times)
}

computation_results <- computation_results %>%
  mutate(method = case_when(
    method == "oat_pcm" ~ "PCM",
    method == "drvs_gcm" ~ "oracle GCM",
    method == "hrt" ~ "HRT",
    method == "split_pcm" ~ "tPCM",
    TRUE ~ method
  ))

# Ensure method levels match across datasets
method_levels <- c("tPCM", "PCM", "oracle GCM", "HRT")
computation_results$method <- factor(computation_results$method, levels = method_levels)

# Create two computation plots (varying n and p)
hrs_n <- plotting %>%
  filter(metric == 'hrs_per_rep' & vary_type == "n") %>%
  mutate(vary = as.numeric(vary), method = factor(method, levels = method_levels)) %>%
  ggplot(aes(x = vary, y = (60 * mean), color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$n))) +
  scale_y_continuous(trans = "log10") +
  labs(x = "n", y = "Runtime (minutes)") +
  theme_bw()

hrs_p <- plotting %>%
  filter(metric == 'hrs_per_rep' & vary_type == "p") %>%
  mutate(vary = as.numeric(vary), method = factor(method, levels = method_levels)) %>%
  ggplot(aes(x = vary, y = (60 * mean), color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(parameter_grid$p))) +
  scale_y_continuous(trans = "log10") +
  labs(x = "p", y = "Runtime (minutes)") +
  theme_bw()

# Combine plots with shared legend at bottom
final_hrs_plot <- hrs_n + hrs_p +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave(paste0(output_dir, "/computation_stat.pdf"), plot = final_hrs_plot, width = 4.5, height = 2.75)
cat("Saved computation_stat.pdf\n")

# ============================================================================
# SECTION 2: Training Proportion Comparison Plots
# ============================================================================

cat("\nLoading data for training proportion comparison...\n")

# Simulation names for proportion comparison
split_pcm_prop_name <- "split_pcm_stat_manu"
oat_pcm_prop_name <- "oat_pcm_stat_manu"

# Load simulation specs (reuse if already loaded)
if (!exists("split_pcm_prop_spec")) {
  split_pcm_prop_spec <- load_sim_spec(split_pcm_prop_name)
  oat_pcm_prop_spec <- load_sim_spec(oat_pcm_prop_name)
}

# Load results
split_pcm_prop_results <- load_sim_results(split_pcm_prop_name, data_dir)
oat_pcm_prop_results <- load_sim_results(oat_pcm_prop_name, data_dir)

# Extract parameter grid
parameter_grid_prop <- split_pcm_prop_spec@parameter_grid %>%
  unnest_wider(X_hyperparams) %>%
  unnest_wider(y_given_X_hyperparams)

# Build vary dataframe
default_row_prop <- parameter_grid_prop[3, ]
vary_prop <- build_vary_dataframe(parameter_grid_prop, vary_parameters, default_row_prop)

# Extract metrics
split_pcm_prop_metrics <- split_pcm_prop_results$metrics %>%
  filter(metric %in% c("fwe", "gb_per_rep", "hrs_per_rep", "power"))

oat_pcm_prop_metrics <- oat_pcm_prop_results$metrics %>%
  filter(metric %in% c("fwe", "gb_per_rep", "hrs_per_rep", "power"))

# Combine and map method names
metrics_prop <- bind_rows(split_pcm_prop_metrics, oat_pcm_prop_metrics) %>%
  unnest_wider(X_hyperparams) %>%
  unnest_wider(y_given_X_hyperparams) %>%
  map_method_names_proportion_gaussian()

# Prepare plotting data
plotting_prop <- metrics_prop %>%
  filter(metric %in% c("fwe", "power", "hrs_per_rep", "gb_per_rep")) %>%
  mutate(metric = factor(metric, levels = c("fwe", "power", "hrs_per_rep", "gb_per_rep"))) %>%
  left_join(vary_prop, by = vary_parameters)

plotting_prop <- add_vary_column(plotting_prop, vary_parameters, errorbar_widths)

# Get alpha level
alpha_level_prop <- split_pcm_prop_spec@fixed_parameters$alpha

# Split into PCM and tPCM
PCM_plot_data <- plotting_prop %>% filter(!grepl("t", method))
tPCM_plot_data <- plotting_prop %>% filter(grepl("t", method))

plot_list <- list(PCM = PCM_plot_data, tPCM = tPCM_plot_data)

# -------------------------------------------------------------------
# Create FWER and Power plots for both PCM and tPCM
# -------------------------------------------------------------------

for (method_type in c("PCM", "tPCM")) {
  cat(paste0("\nCreating ", method_type, " proportion comparison plots...\n"))

  plotting_data <- plot_list[[method_type]]

  # FWER plots
  fwe_data_prop <- plotting_data %>% filter(metric == 'fwe')

  fwe_amplitude_prop <- create_errorbar_plot(
    fwe_data_prop %>% filter(vary_type == "amplitude"),
    sort(unique(parameter_grid_prop$amplitude)),
    seq(0.02, 0.08, 0.02),
    "amplitude",
    alpha_level = alpha_level_prop
  )

  fwe_n_prop <- create_errorbar_plot(
    fwe_data_prop %>% filter(vary_type == "n"),
    sort(unique(parameter_grid_prop$n)),
    seq(0.02, 0.08, 0.02),
    "n",
    alpha_level = alpha_level_prop
  )

  fwe_p_prop <- create_errorbar_plot(
    fwe_data_prop %>% filter(vary_type == "p"),
    sort(unique(parameter_grid_prop$p)),
    seq(0.02, 0.08, 0.02),
    "p",
    alpha_level = alpha_level_prop
  )

  fwe_rho_prop <- create_errorbar_plot(
    fwe_data_prop %>% filter(vary_type == "rho"),
    sort(unique(parameter_grid_prop$rho)),
    seq(0.02, 0.08, 0.02),
    "rho",
    alpha_level = alpha_level_prop
  )

  fwe_s_prop <- create_errorbar_plot(
    fwe_data_prop %>% filter(vary_type == "s"),
    sort(unique(parameter_grid_prop$s)),
    seq(0.02, 0.08, 0.02),
    "s",
    alpha_level = alpha_level_prop,
    show_legend = TRUE
  )

  final_fwe_plot_prop <- create_grid_plot(
    list(fwe_amplitude_prop, fwe_n_prop, fwe_p_prop, fwe_rho_prop, fwe_s_prop),
    "FWER"
  )

  ggsave(
    paste0(output_dir, "/fwer_", method_type, "_choose_proportion.pdf"),
    plot = final_fwe_plot_prop,
    width = 6,
    height = 4.5
  )
  cat(paste0("Saved fwer_", method_type, "_choose_proportion.pdf\n"))

  # Power plots
  power_data_prop <- plotting_data %>% filter(metric == 'power')

  power_amplitude_prop <- create_errorbar_plot(
    power_data_prop %>% filter(vary_type == "amplitude"),
    sort(unique(parameter_grid_prop$amplitude)),
    seq(0.15, 0.9, 0.15),
    "amplitude"
  )

  power_n_prop <- create_errorbar_plot(
    power_data_prop %>% filter(vary_type == "n"),
    sort(unique(parameter_grid_prop$n)),
    seq(0.15, 0.9, 0.15),
    "n"
  )

  power_p_prop <- create_errorbar_plot(
    power_data_prop %>% filter(vary_type == "p"),
    sort(unique(parameter_grid_prop$p)),
    seq(0.15, 0.9, 0.15),
    "p"
  )

  power_rho_prop <- create_errorbar_plot(
    power_data_prop %>% filter(vary_type == "rho"),
    sort(unique(parameter_grid_prop$rho)),
    seq(0.15, 0.9, 0.15),
    "rho"
  )

  power_s_prop <- create_errorbar_plot(
    power_data_prop %>% filter(vary_type == "s"),
    sort(unique(parameter_grid_prop$s)),
    seq(0.15, 0.9, 0.15),
    "s",
    show_legend = TRUE
  )

  final_power_plot_prop <- create_grid_plot(
    list(power_amplitude_prop, power_n_prop, power_p_prop, power_rho_prop, power_s_prop),
    "Power"
  )

  ggsave(
    paste0(output_dir, "/power_", method_type, "_choose_proportion.pdf"),
    plot = final_power_plot_prop,
    width = 6,
    height = 4.5
  )
  cat(paste0("Saved power_", method_type, "_choose_proportion.pdf\n"))
}

cat("\n=== Gaussian simulation plots complete! ===\n")
cat("Generated 6 figures in", output_dir, "\n")
