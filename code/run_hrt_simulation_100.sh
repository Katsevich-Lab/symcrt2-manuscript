#!/bin/bash
#$ -m e -M jzhang17@wharton.upenn.edu
#$ -l m_mem_free=100G

# Define lists of arguments
p_list=(100)
method_list=(hrt)

# Iterate over each combination of arguments
for p in "${p_list[@]}"; do
  for method in "${method_list[@]}"; do
    # Run Rscript with current combination of arguments
    Rscript run_one_simulation.R "$p" "$method"
  done
done