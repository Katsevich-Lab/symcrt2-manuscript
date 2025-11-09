# Plot Real Data Analysis Results
# Produces 1 figure:
# - data_analysis_plot.pdf (Fig 6)
# Three violin plots showing FDR rejections, FWER rejections, and runtime

# Load libraries
library(kableExtra)
library(dplyr)
library(tidyr)
library(patchwork)
library(cowplot)
library(ggplot2)
library(tidyverse)
library(scales)

# Output directory
output_dir <- "manuscript/figures"

# Get data directory
data_dir <- .get_config_path("LOCAL_SYMCRT2_DATA_DIR")

cat("Creating data analysis plot...\n")

# ============================================================================
# Configuration
# ============================================================================

# Method names
split_pcm_name <- "split_pcm"
gcm_name <- "gcm"
hrt_name <- "hrt"
oat_pcm_name <- "oat_pcm"
knockoff_name <- "knockoff"

# Parameters
iter <- 25
alpha <- 0.1
p <- 164

# ============================================================================
# Load Data
# ============================================================================

cat("Loading data...\n")

# Load results for each method
# Note: split_pcm and oat_pcm use training proportion 0.3
split_pcm_results <- readRDS(paste0(
  data_dir, "/private/results/random_forest_binary_fdr/",
  split_pcm_name, "/", split_pcm_name, "_results.rds"
))["0.3", ]

gcm_results <- readRDS(paste0(
  data_dir, "/private/results/random_forest_binary_fdr/",
  gcm_name, "/", gcm_name, "_results.rds"
))

knockoff_results <- readRDS(paste0(
  data_dir, "/private/results/random_forest_binary_fdr/",
  knockoff_name, "/", knockoff_name, "_results.rds"
))

# HRT has separate files for each iteration
hrt_results <- vector(mode = "list", length = iter)
for (i in 1:iter) {
  hrt_results[[i]] <- readRDS(paste0(
    data_dir, "/private/results/random_forest_binary_fdr/",
    hrt_name, "/", hrt_name, "_", i, "_results.rds"
  ))
}

oat_pcm_results <- readRDS(paste0(
  data_dir, "/private/results/random_forest_binary_fdr/",
  oat_pcm_name, "/", oat_pcm_name, "_results.rds"
))["0.3", ]

# ============================================================================
# Helper Functions
# ============================================================================

# Multiple testing correction function factory
correction_alpha_function <- function(method = "BH", alpha = 0.1) {
  correction_alpha <- function(result) {
    p_values <- result$p_values
    p_values_adjusted <- p.adjust(p_values, method = method)
    nonnulls <- which(p_values_adjusted <= alpha)
    return(nonnulls)
  }
  return(correction_alpha)
}

count <- function(result) {
  return(length(result))
}

count_nonnulls <- function(result) {
  return(length(result$nonnulls))
}

tally_nonnulls <- function(result) {
  return(seq(1, p) %in% result$nonnulls)
}

get_time <- function(result) {
  return(result$time)
}

# ============================================================================
# Apply Multiple Testing Corrections
# ============================================================================

cat("Applying multiple testing corrections...\n")

BH_alpha <- correction_alpha_function(alpha = alpha)
bonferroni_alpha <- correction_alpha_function(method = "bonferroni", alpha = alpha)

# Apply BH correction
split_pcm_BH <- sapply(split_pcm_results, BH_alpha)
gcm_BH <- sapply(gcm_results, BH_alpha)
hrt_BH <- sapply(hrt_results, BH_alpha)
oat_pcm_BH <- sapply(oat_pcm_results, BH_alpha)

# Apply Bonferroni correction
split_pcm_bonferroni <- sapply(split_pcm_results, bonferroni_alpha)
gcm_bonferroni <- sapply(gcm_results, bonferroni_alpha)
hrt_bonferroni <- sapply(hrt_results, bonferroni_alpha)
oat_pcm_bonferroni <- sapply(oat_pcm_results, bonferroni_alpha)

# ============================================================================
# Plot 1: FDR Rejections (BH correction)
# ============================================================================

cat("Creating FDR rejections plot...\n")

fdr_results_list <- list(
  tPCM = unlist(sapply(split_pcm_BH, count)),
  Knockoffs = unlist(sapply(knockoff_results, count_nonnulls)),
  HRT = unlist(sapply(hrt_BH, count)),
  PCM = unlist(sapply(oat_pcm_BH, count)),
  tGCM = unlist(sapply(gcm_BH, count))
)

fdr_summary <- stack(fdr_results_list)
colnames(fdr_summary) <- c("Rejections", "Method")

# Compute quantiles
quantiles_fdr <- fdr_summary %>%
  group_by(Method) %>%
  summarize(q50 = quantile(Rejections, 0.5)) %>%
  tidyr::pivot_longer(cols = starts_with("q"), names_to = "quantile", values_to = "value")

fdr_plot <- ggplot(fdr_summary, aes(x = Method, y = Rejections)) +
  geom_violin(fill = "lightblue", color = "black", scale = "width") +
  stat_summary(aes(color = "Mean"), fun = mean, geom = "point", shape = 95, size = 8) +
  geom_point(data = quantiles_fdr, aes(y = value, color = "Median"), shape = 95, size = 8) +
  scale_color_manual(name = NULL, values = c("Mean" = "red", "Median" = "blue")) +
  labs(x = NULL) +
  ggtitle("Rejections at FDR <= 0.1") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1)
  )

# ============================================================================
# Plot 2: FWER Rejections (Bonferroni correction)
# ============================================================================

cat("Creating FWER rejections plot...\n")

fwer_results_list <- list(
  tPCM = unlist(sapply(split_pcm_bonferroni, count)),
  HRT = unlist(sapply(hrt_bonferroni, count)),
  PCM = unlist(sapply(oat_pcm_bonferroni, count)),
  tGCM = unlist(sapply(gcm_bonferroni, count))
)

fwer_summary <- stack(fwer_results_list)
colnames(fwer_summary) <- c("Rejections", "Method")

# Compute quantiles
quantiles_fwer <- fwer_summary %>%
  group_by(Method) %>%
  summarize(q50 = quantile(Rejections, 0.5)) %>%
  tidyr::pivot_longer(cols = starts_with("q"), names_to = "quantile", values_to = "value")

fwer_plot <- ggplot(fwer_summary, aes(x = Method, y = Rejections)) +
  geom_violin(fill = "lightblue", color = "black", scale = "width") +
  stat_summary(aes(color = "Mean"), fun = mean, geom = "point", shape = 95, size = 8) +
  geom_point(data = quantiles_fwer, aes(y = value, color = "Median"), shape = 95, size = 8) +
  scale_color_manual(name = "", values = c("Mean" = "red", "Median" = "blue")) +
  labs(x = NULL) +
  ggtitle("Rejections at FWER <= 0.1") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    legend.position = "none"
  )

# ============================================================================
# Plot 3: Computation Time
# ============================================================================

cat("Creating runtime plot...\n")

time_list <- list(
  tPCM = 60 * sapply(split_pcm_results, get_time),
  Knockoffs = sapply(knockoff_results, get_time),
  HRT = 60 * 60 * sapply(hrt_results, get_time),
  PCM = 60 * sapply(oat_pcm_results, get_time),
  tGCM = 60 * sapply(gcm_results, get_time)
)

time_summary <- stack(time_list)
colnames(time_summary) <- c("Seconds", "Method")

quantiles_time <- time_summary %>%
  group_by(Method) %>%
  summarize(q50 = quantile(Seconds, 0.5)) %>%
  tidyr::pivot_longer(cols = starts_with("q"), names_to = "quantile", values_to = "value")

time_plot <- ggplot(time_summary, aes(x = Method, y = Seconds)) +
  geom_violin(fill = "lightblue", color = "black", scale = "width") +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(y = "Runtime (seconds)", x = NULL) +
  stat_summary(aes(color = "Mean"), fun = mean, geom = "point", shape = 95, size = 8) +
  geom_point(data = quantiles_time, aes(y = value, color = "Median"), shape = 95, size = 8) +
  scale_color_manual(name = "", values = c("Mean" = "red", "Median" = "blue")) +
  ggtitle("Runtime") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    legend.position = "none"
  )

# ============================================================================
# Combine Plots
# ============================================================================

cat("Combining plots...\n")

# Keep legend in first plot (top-right), remove from others
fdr_plot_with_legend <- fdr_plot
fwer_plot_clean <- fwer_plot
time_plot_clean <- time_plot

# Arrange in 1x3 grid (horizontal)
data_analysis_plot <- fdr_plot_with_legend + fwer_plot_clean + time_plot_clean

# Save
ggsave(paste0(output_dir, "/data_analysis_plot.pdf"),
       plot = data_analysis_plot, width = 6.25, height = 2.5)

cat("Saved data_analysis_plot.pdf\n")
cat("\n=== Data analysis plot complete! ===\n")
