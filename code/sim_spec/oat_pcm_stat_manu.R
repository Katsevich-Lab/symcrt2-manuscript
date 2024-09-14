# name of the simulation
sim_name <- "oat_pcm_stat_manu"
sim_spec_fp <- paste0("code/sim_spec/sim_spec_", sim_name, ".rds")

# Create the parameter grid
# The default values for the simulation
seed = 1234
transforms <- rep(list(
  function(x) (x - 0.3)^2 / sqrt(2),
  function(x) -cos(x)
), 100)
baseline_values = list(n = 1200,  # sample size
                       p = 50,  # dimension of Z
                       s = 12,
                       joint_X = "AR", y_given_X = "gam", 
                       X_hyperparams = list(list(rho = 0.5)),
                       y_given_X_hyperparams = list(list(family = "gaussian", 
                                                         transforms = transforms)),
                       amplitude = 0.25)
varying_values = list(n = 800 + 200 * seq(0, 4, 1),  # sample size
                      p = seq(30, 70, 10),  # dimension of Z
                      s = seq(4, 20, 4),
                      joint_X = "AR", y_given_X = "gam", 
                      X_hyperparams = list(list(rho = 0.2),list(rho = 0.35),
                                           list(rho = 0.5),
                                           list(rho = 0.65),list(rho = 0.8)),
                      y_given_X_hyperparams = list(list(family = "gaussian",
                                                        transforms = transforms)),
                      amplitude = seq(0.15,0.35, 0.05))
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
  "oat_pcm bam bam_cs 0.3 FALSE bonferroni 1 FALSE",
  "oat_pcm bam bam_cs 0.4 FALSE bonferroni 1 FALSE",
  "oat_pcm bam bam_cs 0.5 FALSE bonferroni 1 FALSE"
)

# Generate the simulatr specified object
simulatr_spec <- symcrt2::create_simspec_object_parameter_grid(alpha = 0.05, seed = 4, B = 400,
                                                               parameter_grid = parameter_grid,
                                                               method_strings = method_strings,
                                                               distribution = "gaussian")


# Save the simulatr specifier object as an RDS
saveRDS(simulatr_spec, sim_spec_fp)
