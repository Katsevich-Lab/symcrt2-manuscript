args <- commandArgs(trailingOnly = TRUE)
p <- as.integer(args[1])
method <- as.character(args[2])
B <- as.integer(args[3])

# 1. Set working directory to top level
# setwd(paste0(.get_config_path("LOCAL_CODE_DIR"), "symcrt2-project"))

# 2. Set up R packages
renv::activate()
renv::restore()
library(simulatr)

# code to create simulatr specifier object for just p and method for one rep
sim_name <- paste(method, p, B, sep = "_")
check <- FALSE
# Create the parameter grid

# The default values for the simulation
seed = 1234
baseline_values = list(n = 2500,  # sample size
                       p = p,  # dimension of Z
                       s = 20,
                       joint_X = "HMM", y_given_X = "interacted", 
                       X_hyperparams = list(list(K = 5, M = 2,
                                                 gamma = 0.9,
                                                 stay_prob = 0.5)),
                       y_given_X_hyperparams = list(list(family = "gaussian", 
                                                         order = 2, decay = 0.2,
                                                         transform = function(x) {cos(x)})),
                       amplitude = 0.8)
varying_values = list(n = 2500,  # sample size
                      p = p,  # dimension of Z
                      s = 20,
                      joint_X = "HMM", y_given_X = "interacted", 
                      X_hyperparams = list(list(K = 5, M = 2, gamma = 0.9,
                                                stay_prob = 0.5)),
                      y_given_X_hyperparams = list(list(family = "gaussian", 
                                                        order = 2, decay = 0.2,
                                                        transform = function(x) {cos(x)})),
                      amplitude = 0.8)
parameter_grid <- symcrt2::create_param_grid_fractional_factorial_list(
  varying_values,
  baseline_values
)

# function that inputs parameters and outputs the ground truth inferential target(s)
get_ground_truth <- function(p, s){
  list(nonnulls = R.utils::withSeed(sample(p, s), seed = seed))
}

# Add a column to parameter grid containing ground truth inferential targets
parameter_grid <- parameter_grid |> simulatr::add_ground_truth(get_ground_truth)

# only keep default since hrt is slow
# parameter_grid <- parameter_grid[3,]

# The methods to compare, banded_precision gcm vs oat gcm
if (method == "hrt") {
  method_strings <- paste(
    "hrt hmm_fastPhase ranger 0.45 FALSE BH", 5 * p 
  )
} else if (method == "split_pcm") {
  method_strings <- c("split_pcm hmm_fastPhase ranger FALSE 0.45 25 FALSE BH")
} else if (method == "oat_pcm") {
  method_strings <- c("oat_pcm ranger_categorical ranger 0.45 FALSE BH NA FALSE FALSE")
} else if (method == "drvs_gcm") {
  method_strings <- c("drvs_gcm oracle oracle FALSE 1 25 BH")
} else if (method == "knockoff") {
  method_strings <- c("knockoff_wrapper hmm_fastPhase random_forest TRUE")
} else if (method == "hrt_mem") {
  method_strings <- paste(
    "hrt_mem hmm_fastPhase ranger 0.45 FALSE BH", p 
  )
}


# Generate the simulatr specified object
simulatr_spec <- symcrt2::create_simspec_object_parameter_grid(alpha = 0.1, seed = 4, B = B,
                                                               parameter_grid = parameter_grid,
                                                               method_strings = method_strings,
                                                               distribution = "gaussian")



# call check_simulatr_specifier_object on the above object
if(check) B_in <- 1 else B_in <- NULL
results <- check_simulatr_specifier_object(simulatr_spec, B_in = B_in, return_data = TRUE, parallel = FALSE)

# extract results and write to file
if(!check){
  # results_dir <- paste0("results/", sim_name)
  results_dir <- paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"), "/private/results/hmm_interacted_computation")
  if(!dir.exists(results_dir)) dir.create(results_dir)
  saveRDS(results, file = paste0(results_dir, "/", sim_name, "_results.rds"))
}

