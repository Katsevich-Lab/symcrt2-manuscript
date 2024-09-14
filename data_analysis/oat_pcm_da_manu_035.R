# Set up R packages
renv::activate()
renv::restore()

library(symcrt2)

name <- "oat_pcm_da_manu_035"

# read in the processed data
data <- readRDS("data_analysis/data.rds")
data <- list(X = data$X, y = data$Y)

# run multiple versions of oat_pcm
start_time <- Sys.time()
oat_pcm_035 <- symcrt2::oat_pcm(data = data, fit_X = symcrt2::fit_y_given_X_bam_cs,
                               training_proportion = 0.35, linear = FALSE,
                               fit_y_given_X = symcrt2::fit_y_given_X_bam_cs,
                               alpha = 0.05, degeneracy_correction = FALSE,
                               multiple_correction = "bonferroni")
end_time <- Sys.time()
oat_pcm_035$time <- end_time - start_time


# combine results
results <- list(oat_pcm_035 = oat_pcm_035)

results_dir <- paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"), "/private/results/", name)
if(!dir.exists(results_dir)) dir.create(results_dir)
saveRDS(results, file = paste0(results_dir, "/", name, "_results.rds"))
