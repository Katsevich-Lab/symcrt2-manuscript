# Plot Computational Comparison
# Produces 1 figure:
# - computation_comparison.pdf (Fig 5)
# Compares computational efficiency between Gaussian (GAM) and HMM (RF) data models

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

cat("Creating computational comparison plot...\n")

# ============================================================================
# PART 1: Gaussian Data Model (Generalized Additive Model)
# ============================================================================

cat("Loading Gaussian computational data...\n")

methods_gaussian <- c("split_pcm", "drvs_gcm", "hrt", "oat_pcm")

computation_results_gaussian <- data.frame(
  method = rep(methods_gaussian, 5),
  p = rep(c(100, 125, 150, 175, 200), each = 4),
  time = rep(0, 20)
)

for (i in 1:nrow(computation_results_gaussian)) {
  method_name <- computation_results_gaussian$method[i]
  p_value <- computation_results_gaussian$p[i]

  result <- readRDS(paste0(
    data_dir, "/private/results/",
    method_name, "_", p_value,
    "/", method_name, "_", p_value, "_results.rds"
  ))
  computation_results_gaussian$time[i] <- unlist(result$method_times)
}

# Map method names
computation_results_gaussian <- computation_results_gaussian %>%
  mutate(method = case_when(
    method == "oat_pcm" ~ "PCM",
    method == "drvs_gcm" ~ "oracle GCM",
    method == "hrt" ~ "HRT",
    method == "split_pcm" ~ "tPCM",
    TRUE ~ method
  ))

# Define consistent color palette for all methods
method_colors <- c(
  "tPCM" = "#F8766D",        # red/salmon
  "PCM" = "#7CAE00",         # green
  "oracle GCM" = "#00BFC4",  # cyan
  "HRT" = "#C77CFF",         # purple
  "Knockoffs" = "#FF61CC"    # pink (only in HMM)
)

# Ensure method levels match across datasets for legend consolidation
# Use all 5 levels in both plots so legends can be merged
method_levels <- c("tPCM", "PCM", "oracle GCM", "HRT", "Knockoffs")
computation_results_gaussian$method <- factor(computation_results_gaussian$method, levels = method_levels)

# Create Gaussian data model plot
hrs_comp_gaussian <- computation_results_gaussian %>%
  ggplot(aes(x = p, y = time / 3600, color = method)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = method_colors, drop = TRUE) +
  labs(x = "p", y = "Runtime (hours)", title = "Generalized additive model") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# ============================================================================
# PART 2: HMM Interacted Data Model (Random Forest)
# ============================================================================

cat("Loading HMM computational data...\n")

hmm_sim_name <- "hmm_interacted_fdr"

# Load simulation spec
hmm_split_pcm_spec <- readRDS(paste0("code/sim_spec/", hmm_sim_name, "/sim_spec_split_pcm.rds"))
hmm_parameter_grid <- hmm_split_pcm_spec@parameter_grid %>% unnest_wider(X_hyperparams)

hmm_method_names <- c("split_pcm", "oat_pcm", "hrt", "knockoff", "gcm")

# Define helper function to load metrics
load_hmm_metrics <- function(method_name) {
  spec_path <- paste0("code/sim_spec/", hmm_sim_name, "/sim_spec_", method_name, ".rds")
  results_path <- paste0(data_dir, "/private/results/", hmm_sim_name, "/",
                         method_name, "/", method_name, "_results.rds")

  simulatr_spec <- readRDS(spec_path)
  results <- readRDS(results_path)

  metrics <- results$metrics %>%
    filter(metric %in% c("fdp", "gb_per_rep", "hrs_per_rep", "power")) %>%
    mutate(method = method_name)

  return(metrics)
}

hmm_combined_metrics <- purrr::map_dfr(hmm_method_names, load_hmm_metrics)

# Reorganize data
hmm_vary_parameters <- c("n", "p", "s", "stay_prob", "amplitude")

# Default row
hmm_default_row <- hmm_parameter_grid[3, ]
hmm_vary <- build_vary_dataframe(hmm_parameter_grid, hmm_vary_parameters, hmm_default_row)

# Process metrics
hmm_metrics <- hmm_combined_metrics %>%
  unnest_wider(X_hyperparams) %>%
  mutate(method = case_when(
    method == "oat_pcm" ~ "PCM",
    method == "gcm" ~ "oracle GCM",
    method == "hrt" ~ "HRT",
    method == "split_pcm" ~ "tPCM",
    method == "knockoff" ~ "Knockoffs",
    TRUE ~ method
  ))

# Set factor levels to match Gaussian plot
hmm_metrics$method <- factor(hmm_metrics$method, levels = method_levels)

# Prepare plotting data
hmm_plotting <- hmm_metrics %>%
  left_join(hmm_vary, by = hmm_vary_parameters)

hmm_plotting$vary <- ""
for (m in 1:nrow(hmm_plotting)) {
  hmm_plotting$vary[m] <- hmm_plotting[m, hmm_plotting$vary_type[m]]
}

# Create HMM p computation plot
hmm_hrs_p <- hmm_plotting %>%
  filter(metric == 'hrs_per_rep' & vary_type == "p") %>%
  mutate(vary = as.numeric(vary)) %>%
  ggplot(aes(x = vary, y = (60 * mean), color = method)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = sort(unique(hmm_parameter_grid$p))) +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(name = NULL, values = method_colors, breaks = method_levels, drop = FALSE) +
  labs(x = "p", y = "Runtime (minutes)", title = "Random forest") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  )

# ============================================================================
# PART 3: Combine Plots
# ============================================================================

cat("Combining plots...\n")

# Extract legend from HMM plot
legend <- cowplot:::get_plot_component(hmm_hrs_p, "guide-box", return_all = TRUE)[[3]]

# Combine plots horizontally without legend
plots_combined <- (hmm_hrs_p + theme(legend.position = "none")) + hrs_comp_gaussian

# Add legend at bottom
combined_computation_plot <- plot_grid(plots_combined, legend, ncol = 1, rel_heights = c(1, 0.15))

# Save
ggsave(paste0(output_dir, "/computation_comparison.pdf"),
       plot = combined_computation_plot, width = 5.25, height = 3.25)

cat("Saved computation_comparison.pdf\n")
cat("\n=== Computational comparison plot complete! ===\n")
