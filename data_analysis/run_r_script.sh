#!/bin/bash
#$ -m e -M jzhang17@wharton.upenn.edu

# Check if an argument is passed
if [ -z "$1" ]; then
  echo "Usage: $0 <R_script_filename>"
  exit 1
fi

# Get the R script filename from the first argument
R_SCRIPT="$1"

# Run the R script using Rscript command
Rscript "$R_SCRIPT"

# Check if Rscript executed successfully
if [ $? -eq 0 ]; then
  echo "R script $R_SCRIPT executed successfully."
else
  echo "Error: Failed to execute $R_SCRIPT."
fi
