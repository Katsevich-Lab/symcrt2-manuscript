# Set up R packages
renv::activate()
renv::restore()

library(symcrt2)

name <- "hrt"
# number of iterations to test stability
iter <- 25
alpha <- 0.1
proportions <- c(0.3, 0.35, 0.4)
results <- matrix(list(), nrow = length(proportions), ncol = iter)
rownames(results) <- as.character(proportions)

# read in the processed data
data <- readRDS("data_analysis/data.rds")
data <- list(X = data$X, y = data$Y)

# run multiple versions of hrt
for (prop in proportions) {
  for (i in 1:iter) {
    start_time <- Sys.time()
    hrt_placeholder <- symcrt2::hrt(data = data, fit_X = symcrt2::fit_X_CV_graphical_lasso,
                                                training_proportion = prop, B = 400,
                                                fit_y_given_X = symcrt2::fit_y_given_X_ranger_binary,
                                                alpha = alpha, degeneracy_correction = FALSE,
                                                multiple_correction = "BH", seed = i)
    end_time <- Sys.time()
    hrt_placeholder$time <- end_time - start_time
    results[[as.character(prop), i]] <- hrt_placeholder
  }
}

results_dir <- paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"), 
                      "/private/results/random_forest_binary_fdr/", name)
if(!dir.exists(results_dir)) dir.create(results_dir)
saveRDS(results, file = paste0(results_dir, "/", name, "_results.rds"))
