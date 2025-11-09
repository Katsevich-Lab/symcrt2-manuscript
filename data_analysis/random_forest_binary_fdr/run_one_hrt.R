# seed
args <- commandArgs(trailingOnly = TRUE)
seed <- as.integer(args[1])

# Set up R packages
renv::activate()
renv::restore()

library(symcrt2)

name <- "hrt"
# number of iterations to test stability
alpha <- 0.1
prop <- 0.3
B <- 3300

# read in the processed data
data <- readRDS("data_analysis/data.rds")
data <- list(X = data$X, y = data$Y)

# run one hrt

start_time <- Sys.time()
hrt_placeholder <- symcrt2::hrt_mem(data = data, fit_X = symcrt2::fit_X_CV_graphical_lasso,
                                    training_proportion = prop, B = B,
                                    fit_y_given_X = symcrt2::fit_y_given_X_ranger_binary,
                                    alpha = alpha, degeneracy_correction = FALSE,
                                    multiple_correction = "BH", seed = seed)
end_time <- Sys.time()
hrt_placeholder$time <- end_time - start_time
results <- hrt_placeholder

results_dir <- paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"), 
                      "/private/results/random_forest_binary_fdr/", name)
if(!dir.exists(results_dir)) dir.create(results_dir)
saveRDS(results, file = paste0(results_dir, "/", name, "_", seed, "_results.rds"))
