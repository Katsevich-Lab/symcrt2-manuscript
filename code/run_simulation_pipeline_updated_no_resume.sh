#######################################################################
#
# Run a simulation on HPCC from within
# base directory, i.e. symcrt2-project/symcrt2-manuscript
#
# This script is a wrapper around the simulatr Nextflow pipeline, documented at 
# https://katsevich-lab.github.io/simulatr/articles/example-remote.html. 
#
# PARAMETERS:
# 
# - sim_name:  (Required) The name of the simulatr specifier file under 
#              code/sim_spec. Note that the simulatr specifier file must already
#              have been created before running this script. I recommend doing so
#              using run_simulation_local.R with check = TRUE.
# - sim_folder_name:  (Required) The name of the folder under 
#              code/sim_spec. Note that the simulatr specifier file must already
#              have been created before running this script. I recommend doing so
#              using run_simulation_local.R with check = TRUE.
# - profile:   (Optional) The Nextflow profile to use on HPCC. The three options
#              are "local", "standard"", and "aws". The "local" option is for 
#.             running the simulation interactively, and primarily useful for 
#              test-runs. The "standard" option is the default, and uses the 
#              usual HPCC resources to run the simulation in batch mode. The 
#              "aws" option uses Gene's AWS resources; consult with him first.
# - B_check:   (Optional) See documentation linked above.
# - B:         (Optional) See documentation linked above.
# - max_gb:    (Optional) See documentation linked above.
# - max_hours: (Optional) See documentation linked above.
# 
# RUNNING THE SCRIPT:
# 
# To run the script interactively, use the command
# 
# bash run_simulation_pipeline.sh --<param-name> <param-value> ...
# 
# To run the script in batch mode, use the command
# 
# echo "bash run_simulation_pipeline.sh --<param-name> <param-value> ..." | qsub -N run_all
# 
# OUTPUT: 
# 
# - The results of the simulation will be written to 
#   LOCAL_SYMCRT2_DATA_DIR/private/results/<sim_folder_name>/<sim_name>/<sim_name>_results.rds.
#######################################################################

# 0. Read simulation parameters from command line
# sim_name=        # Name of the simulation (no default)
# sim_folder_name=        # Name of the simulation (no default)
profile="standard" # Nextflow profile (default standard)
B_check=3          # Number of replicates to use for benchmarking (default 3)
B=0                # Number of replicates for main simulation (default read from sim spec obj)
max_gb=7.5         # Maximum number of GB per process (default 7.5)
max_hours=4        # Maximum number of hours per process (default 4)
while [ $# -gt 0 ]; do
    if [[ $1 == "--"* ]]; then
        v="${1/--/}"
        declare "$v"="$2"
        shift
    fi
    shift
done

if [ -z "$sim_name" ]; then
  echo "Error: The simulation name needs to be given via the\
 command-line argument sim_name."
  exit
fi

if [ -z "$sim_folder_name" ]; then
  echo "Error: The simulation folder name needs to be given via the\
 command-line argument sim_folder_name."
  exit
fi

echo "Launching the "$sim_name" simulation with profile = "$profile", B_check = "\
$B_check", B = "$B", max_gb = "$max_gb", max_hours = "$max_hours"..."

# 1. Set up directories/paths
source ~/.research_config
if [ -z "$NXF_WORK" ]; then
  echo "Error: The environment variable \$NXF_WORK needs to be set"
  exit
fi
simspec_filename=$LOCAL_CODE_DIR/symcrt2-project/code/sim_spec/$sim_folder_name/sim_spec_$sim_name.rds
if [ -z "$simspec_filename" ]; then
  echo "Error: The simulatr specifier object was not found."
  exit
fi

# output_dir=$LOCAL_CODE_DIR/symcrt2-project/results/$sim_name
output_dir=$LOCAL_SYMCRT2_DATA_DIR/private/results/$sim_folder_name/$sim_name
mkdir -p $output_dir

# 2. Set up R packages
Rscript -e '
renv::activate()
renv::restore()'
RENV_R_LIBS_USER=$(Rscript -e '
renv::activate(getwd()) |>
  capture.output() |>
  invisible() 
cat(paste0(.libPaths(), collapse = ":"))')
echo "env.R_LIBS_USER = \"$RENV_R_LIBS_USER\"" > nextflow.config

# 3. Run the simulation
# obtain simulation string from simspec_filename
output_filename=$sim_name"_results.rds"
if [ ! -f "$output_dir/$output_filename" ]; then
  echo "Running the "$sim_name" simulation..."
  nextflow pull katsevich-lab/simulatr-pipeline
  nextflow run katsevich-lab/simulatr-pipeline \
    --simulatr_specifier_fp $simspec_filename \
    --result_dir $output_dir \
    --result_file_name $output_filename \
    --B_check $B_check \
    --B $B \
    --max_gb $max_gb \
    --max_hours $max_hours \
    -profile $profile \
    -with-trace $output_dir/$sim_name"_trace.txt"
  wait
  if [ -f "$output_dir/$output_filename" ]; then
      rm -r $NXF_WORK/*
  else
      exit
  fi
else
  echo $output_filename" already exists"
fi

echo "Done."