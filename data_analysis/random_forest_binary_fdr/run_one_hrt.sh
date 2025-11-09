#!/bin/bash
#$ -m e -M jzhang17@wharton.upenn.edu

seed=$1

Rscript data_analysis/random_forest_binary_fdr/run_one_hrt.R "$seed"