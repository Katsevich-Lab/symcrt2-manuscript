# Set up R packages
renv::activate()
renv::restore()

library(symcrt2)

name <- "gcm_da_manu"

# read in the processed data
data <- readRDS("data_analysis/data.rds")
data <- list(X = data$X, y = data$Y)

start_time <- Sys.time()
drvs_gcm_5 <- symcrt2::drvs_gcm(data = data, fit_X = symcrt2::fit_X_CV_graphical_lasso,
                                    linear = FALSE, B = 25, K = 5,
                                    fit_y_given_X = symcrt2::fit_y_given_X_bam_cs,
                                    alpha = 0.05, 
                                    multiple_correction = "bonferroni")
end_time <- Sys.time()
drvs_gcm_5$time <- end_time - start_time

# combine results
results <- list(drvs_gcm_5 = drvs_gcm_5)

results_dir <- paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"), "/private/results/", name)
if(!dir.exists(results_dir)) dir.create(results_dir)
saveRDS(results, file = paste0(results_dir, "/", name, "_results.rds"))


