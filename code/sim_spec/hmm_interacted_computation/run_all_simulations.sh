#!/bin/bash
#$ -m e -M jzhang17@wharton.upenn.edu
#$ -l m_mem_free=2G

# Define lists of arguments
# p_list=(100 125 150 175 200)
p_list=(800 900 1000 1100 1200)
method_list=(split_pcm oat_pcm drvs_gcm knockoff)

# Iterate over each combination of arguments
for p in "${p_list[@]}"; do
  for method in "${method_list[@]}"; do
    # Run Rscript with current combination of arguments
    echo "Submitting job for p=$p, method=$method"
    qsub -l m_mem_free=8G code/sim_spec/hmm_interacted_computation/run_one_simulation.sh "$p" "$method" "5"
  done
done