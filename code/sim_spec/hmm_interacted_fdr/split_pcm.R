# name of the simulation
sim_name <- "split_pcm"
sim_spec_fp <- paste0("code/sim_spec/hmm_interacted_fdr/sim_spec_", sim_name, ".rds")

# Create the parameter grid
# The default values for the simulation
seed = 1234
baseline_values = list(n = 2500,  # sample size
                       p = 50,  # dimension of Z
                       s = 20,
                       joint_X = "HMM", y_given_X = "interacted", 
                       X_hyperparams = list(list(K = 5, M = 2,
                                                 gamma = 0.9,
                                                 stay_prob = 0.5)),
                       y_given_X_hyperparams = list(list(family = "gaussian", 
                                                         order = 2, decay = 0.2,
                                                         transform = function(x) {cos(x)})),
                       amplitude = 0.80)
varying_values = list(n = 2000 + 250 * seq(0, 4, 1),  # sample size
                      p = seq(30, 70, 10),  # dimension of Z
                      s = seq(12, 28, 4),
                      joint_X = "HMM", y_given_X = "interacted", 
                      X_hyperparams = list(list(K = 5, M = 2, gamma = 0.9,
                                                stay_prob = 0.35),
                                           list(K = 5, M = 2, gamma = 0.9,
                                                stay_prob = 0.425),
                                           list(K = 5, M = 2, gamma = 0.9,
                                                stay_prob = 0.5),
                                           list(K = 5, M = 2, gamma = 0.9,
                                                stay_prob = 0.575),
                                           list(K = 5, M = 2, gamma = 0.9,
                                                stay_prob = 0.65)),
                      y_given_X_hyperparams = list(list(family = "gaussian", 
                                                        order = 2, decay = 0.2,
                                                        transform = function(x) {cos(x)})),
                      amplitude = round(seq(0.7, 0.9, 0.05), 2))
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

# The methods to compare, banded_precision gcm vs oat gcm
method_strings <- c(
  "split_pcm hmm_fastPhase ranger FALSE 0.45 25 FALSE BH"
)


# Generate the simulatr specified object
simulatr_spec <- symcrt2::create_simspec_object_parameter_grid(alpha = 0.1, seed = 4, B = 400,
                                                               parameter_grid = parameter_grid,
                                                               method_strings = method_strings,
                                                               distribution = "gaussian")


# Save the simulatr specifier object as an RDS
saveRDS(simulatr_spec, sim_spec_fp)
