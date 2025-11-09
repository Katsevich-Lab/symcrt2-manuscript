#!/bin/bash
#$ -m e -M jzhang17@wharton.upenn.edu

p=$1
method=$2
B=$3

echo "Running with p=$p and method=$method"
Rscript code/sim_spec/hmm_interacted_computation/run_one_simulation.R "$p" "$method" "$B"

