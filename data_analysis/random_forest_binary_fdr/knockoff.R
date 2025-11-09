# Set up R packages
renv::activate()
renv::restore()

library(symcrt2)

name <- "knockoff"
# number of iterations to test stability
iter <- 25
alpha <- 0.1
results <- vector(mode = "list", iter)


# read in the processed data
data <- readRDS("data_analysis/data.rds")
# data <- list(X = data$X, y = data$Y)
data <- list(X = data$X, y = as.factor(data$Y))



for (i in 1:iter) {
  start_time <- Sys.time()
  knockoff_placeholder <- symcrt2::knockoff_wrapper(data = data, 
    fit_X = symcrt2::fit_X_CV_graphical_lasso,
    fit_y_given_X = knockoff::stat.random_forest, alpha = alpha,  seed = i)
  end_time <- Sys.time()
  knockoff_placeholder$time <- end_time - start_time
  results[[i]] <- knockoff_placeholder
}

results_dir <- paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"), 
                      "/private/results/random_forest_binary_fdr/", name)
if(!dir.exists(results_dir)) dir.create(results_dir)
saveRDS(results, file = paste0(results_dir, "/", name, "_results.rds"))
