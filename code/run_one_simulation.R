args <- commandArgs(trailingOnly = TRUE)
p <- as.integer(args[1])
method <- as.character(args[2])

# 1. Set working directory to top level
# setwd(paste0(.get_config_path("LOCAL_CODE_DIR"), "symcrt2-project"))

# 2. Set up R packages
renv::activate()
renv::restore()
library(simulatr)

# code to create simulatr specifier object for just p and method for one rep
sim_name <- paste(method, p, sep = "_")
check <- FALSE
# Create the parameter grid

# The default values for the simulation
seed = 1234
transforms <- rep(list(
  function(x) (x - 0.3)^2 / sqrt(2),
  function(x) -cos(x)
), 100)
baseline_values = list(n = 2500,  # sample size
                       p = p,  # dimension of Z
                       s = 15,
                       joint_X = "AR", y_given_X = "gam", 
                       X_hyperparams = list(list(rho = 0.5)),
                       y_given_X_hyperparams = list(list(family = "gaussian", 
                                                         transforms = transforms)),
                       amplitude = 0.25)
varying_values = list(n = 2500,  # sample size
                      p = p,  # dimension of Z
                      s = 15,
                      joint_X = "AR", y_given_X = "gam", 
                      X_hyperparams = list(list(rho = 0.5)),
                      y_given_X_hyperparams = list(list(family = "gaussian",
                                                        transforms = transforms)),
                      amplitude = 0.25)
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
    "hrt banded_precision bam_cs 0.4 FALSE bonferroni", 5 * p / 0.05
  )
} else if (method == "split_pcm") {
  method_strings <- c("split_pcm banded_precision bam_cs FALSE 0.4 25 FALSE bonferroni")
} else if (method == "oat_pcm") {
  method_strings <- c("oat_pcm bam bam_cs 0.4 FALSE bonferroni 1 FALSE")
} else if (method == "drvs_gcm") {
  method_strings <- c("drvs_gcm oracle oracle FALSE 1 25 bonferroni")
}



# Generate the simulatr specified object
simulatr_spec <- symcrt2::create_simspec_object_parameter_grid(alpha = 0.05, seed = 4, B = 1,
                                                               parameter_grid = parameter_grid,
                                                               method_strings = method_strings,
                                                               distribution = "gaussian")



# call check_simulatr_specifier_object on the above object
if(check) B_in <- 1 else B_in <- NULL
results <- check_simulatr_specifier_object(simulatr_spec, B_in = B_in, return_data = TRUE, parallel = FALSE)

# extract results and write to file
if(!check){
  # results_dir <- paste0("results/", sim_name)
  results_dir <- paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"), "/private/results/", sim_name)
  if(!dir.exists(results_dir)) dir.create(results_dir)
  saveRDS(results, file = paste0(results_dir, "/", sim_name, "_results.rds"))
}

