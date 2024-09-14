# load libraries
library(tidyverse)
library(kableExtra)
library(dplyr)
library(tidyr)
library(stringr)
library(patchwork)
library(cowplot)

# computation part
methods <- c("split_pcm", "drvs_gcm", "hrt", "oat_pcm")

computation_results <- data.frame(method = rep(methods, 5),
                                  p = rep(c(100, 125, 150, 175, 200), each = 4),
                                  time = rep(0, 20))
names(computation_results) <- c("method", "p", "time (sec)")


for (i in 1:nrow(computation_results)) {
  placeholder_method <- computation_results$method[i]
  placeholder_p <- computation_results$p[i]
  
  placeholder <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/",  
                                placeholder_method, "_", placeholder_p
                                , "/", placeholder_method, "_", ... = placeholder_p, "_results.rds"))
  computation_results$time[i] <- unlist(placeholder$method_times)
  
}

computation_results <- computation_results %>% mutate(method = case_when(
  method == "oat_pcm" ~ "PCM",
  method == "drvs_gcm"  ~ "oracle GCM",
  method == "hrt" ~ "HRT",
  method == "split_pcm" ~ "tPCM",
  TRUE ~ method
))

final_plot <- computation_results %>%
  ggplot(aes(x = p, y = time / 60, color = method)) +
  geom_point() + 
  geom_line() + 
  scale_y_continuous(trans = "log10") +
  labs(x = "p", y = "Minutes per rep", title = "Computational Efficiency") + theme_bw() + 
  theme(legend.position = "none")

# power part with n increasing
# enter the simulation name
split_pcm_name <- "split_pcm_stat_manu"
oat_pcm_name <- "oat_pcm_stat_manu"
gcm_name <- "gcm_stat_manu"
hrt_name <- "hrt_stat_manu"

# load simulatr specs
split_pcm_simulatr_spec <- readRDS(paste0(
                                          "code/sim_spec/sim_spec_", split_pcm_name, ".rds"))
oat_pcm_simulatr_spec <- readRDS(paste0(
                                        "code/sim_spec/sim_spec_", oat_pcm_name, ".rds"))
gcm_simulatr_spec <- readRDS(paste0(
                                    "code/sim_spec/sim_spec_", gcm_name, ".rds"))
hrt_simulatr_spec <- readRDS(paste0(
                                    "code/sim_spec/sim_spec_", hrt_name, ".rds"))

parameter_grid <- split_pcm_simulatr_spec@parameter_grid

# load results
hrt_results <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/",  
                              hrt_name, "/", hrt_name, "_results.rds"))
split_pcm_results <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/",  
                                    split_pcm_name, "/", split_pcm_name, "_results.rds"))
oat_pcm_results <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/", 
                                  oat_pcm_name, "/", oat_pcm_name, "_results.rds"))
gcm_results <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/", 
                              gcm_name, "/", gcm_name, "_results.rds"))

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
split_pcm_metrics <- split_pcm_results$metrics |> filter(method == "split_pcm_banded_precision_bam_cs_2" & 
                                                           metric %in% c("fwe","gb_per_rep","hrs_per_rep","power"))
oat_pcm_metrics <- oat_pcm_results$metrics |> filter(method == "oat_pcm_bam_bam_cs_1" & 
                                                       metric %in% c("fwe","gb_per_rep","hrs_per_rep","power"))
gcm_metrics <- gcm_results$metrics |> filter(metric %in% c("fwe","gb_per_rep","hrs_per_rep","power"))
hrt_metrics <- hrt_results$metrics |> filter(metric %in% c("fwe","gb_per_rep","hrs_per_rep","power"))
# Reorder/reorganize data so that the relevant metrics are plotted in the 
# correct order.
metrics <- bind_rows(split_pcm_metrics,oat_pcm_metrics,gcm_metrics,hrt_metrics) %>% unnest_wider(X_hyperparams) %>% unnest_wider(y_given_X_hyperparams) 
metrics <- metrics %>% mutate(method = case_when(
  method == "oat_pcm_bam_bam_cs_1" ~ "PCM",
  method == "drvs_gcm_oracle_oracle_1"  ~ "oracle GCM",
  method == "hrt_banded_precision_bam_cs_1" ~ "HRT",
  method == "split_pcm_banded_precision_bam_cs_2" ~ "tPCM",
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

power_n <- plotting |> filter(metric == 'power' & vary_type == "n") |> 
  mutate(vary = as.numeric(vary)) |>
  ggplot(aes(x = vary, y = mean, color = method))  +
  geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$n))) +
  scale_y_continuous(breaks = seq(0.15,0.9,0.15)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  labs(x = "n", y = "Power", title = "Statistical Efficiency") + theme_bw() + theme(legend.position = "none")

# get legend
dummy <- plotting |> filter(metric == 'power' & vary_type == "n") |> 
  mutate(vary = as.numeric(vary)) |>
  ggplot(aes(x = vary, y = mean, color = method))  +
  geom_point() + geom_line() + scale_x_continuous(breaks = sort(unique(parameter_grid$n))) +
  scale_y_continuous(breaks = seq(0.15,0.9,0.15)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se, width = errorbar_width)) +
  labs(x = "n") + theme_bw() + labs(color = NULL) + 
  theme(legend.text = element_text(size = 10), legend.position = "bottom", legend.direction = "horizontal",
  legend.background = element_rect(fill = "transparent", colour = NA))
# no longer works with new cowplot
# legend <- get_legend(dummy)
legend = cowplot::get_plot_component(dummy, 'guide-box-bottom', return_all = TRUE)
cowplot::ggdraw(legend)

# put plots and legend together, old version 
# final_fig_1_old <- plot_grid(final_plot, power_n, legend, ncol = 3, rel_widths = c(0.8, 0.8, 0.25))

# put plots and legend together with legend on bottom
combined_plot <- plot_grid(final_plot, power_n, ncol = 2, align = "v")
final_fig_1<- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, 0.2))
# Print the final plot
print(final_fig_1)

ggsave("manuscript/figures/figure_1.pdf", plot = final_fig_1, width = 5.5, height = 3.5)




