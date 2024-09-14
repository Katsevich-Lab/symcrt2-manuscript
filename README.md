Reproducing the simulations and real data analysis reported in \`Doubly
robust and computationally efficient high-dimensional variable
selection’
================
Abhinav Chakraborty, Jeffrey Zhang, Eugene Katsevich

This repository contains code to reproduce the analyses reported in the
paper “Doubly robust and computationally efficient high-dimensional
variable selection” (arXiv, 2024).

# Dependencies

Ensure that your system has the required software dependencies.

- R version 4.2.2 or higher
- Nextflow version 23.10.1 or higher

# Get started

First, clone the `symcrt2-manuscript` repository onto your machine.

    git clone git@github.com:Katsevich-Lab/symcrt2-manuscript.git

We used a config file to increase the portability of our code across
machines. Create a config file called `.research_config` in your home
directory.

    cd
    touch ~/.research_config

Define the following variable within this file:

- `LOCAL_SYMCRT2_DATA_DIR`: the location of the directory in which to
  store simulation results.
- `LOCAL_EXTERNAL_DATA_DIR`: the location of the directory in which to
  store external real data.

The contents of the `.research_config` file should look like something
along the following lines.

    LOCAL_INTERNAL_DATA_DIR="/Users/jeffreyzhang/data/projects/"
    LOCAL_SYMCRT2_DATA_DIR=$LOCAL_INTERNAL_DATA_DIR"symcrt2/"
    LOCAL_EXTERNAL_DATA_DIR="/Users/jeffreyzhang/data/external/"

Next, create an .Rprofile file in your home directory (if you have not
yet done so).

    cd
    touch .Rprofile

Add the following command to your .Rprofile.

    .get_config_path <- function(dir_name) {
      cmd <- paste0("source ~/.research_config; echo $", dir_name)
      system(command = cmd, intern = TRUE)
    }

# Simulations

## Download simulation results data

Next, we recommend downloading the simulation results data from Dropbox,
so that you can reproduce the figures without having to rerun the
simulations. The data are stored in .rds format. Download the `results`
directory from here: [Dropbox results
repository](https://www.dropbox.com/scl/fo/qz1ctahx7tn5i2barich9/ABYSILth4DWkAyzqCc9umkU?rlkey=7lp7suuzvd126vdc4jxexynv5&dl=0)
and place the results directory into LOCAL_SYMCRT2_DATA_DIR/private.
This can also be done using the following commands: First,

    source ~/.research_config
    cd $LOCAL_SYMCRT2_DATA_DIR"/private"
    wget --max-redirect=20 -O download.zip https://www.dropbox.com/scl/fo/qz1ctahx7tn5i2barich9/ABYSILth4DWkAyzqCc9umkU?rlkey=7lp7suuzvd126vdc4jxexynv5&dl=1

Then, execute

     unzip -o download.zip

If you would like to rerun the simulations from scratch, do not download
the results and instead follow the steps in the next section.

## Run the Nextflow pipelines

Navigate to the symcrt2-manuscript directory. All scripts below must be
executed from this directory.

Also, for the commands below, depending on the limits of your cluster,
you may need to set the max_gb and max_hours parameters differently. The
defaults are 7.5 and 4, respectively.

    # tower PCM statistical simulation:
    echo "bash code/run_simulation_pipeline.sh --sim_name split_pcm_stat_manu" | qsub -N run_all

    # PCM statistical simulation:
    echo "bash code/run_simulation_pipeline.sh --sim_name oat_pcm_stat_manu" | qsub -N run_all

    # oracle GCM statistical simulation:
    echo "bash code/run_simulation_pipeline.sh --sim_name gcm_stat_manu" | qsub -N run_all

    # HRT statistical simulation:
    echo "bash code/run_simulation_pipeline.sh --sim_name hrt_stat_manu" | qsub -N run_all

    # tower PCM, PCM, oracle GCM computational simulations:
    qsub code/run_all_simulations.sh 

    # HRT computational simulations:
    qsub code/run_hrt_simulation_100.sh 
    qsub code/run_hrt_simulation_125.sh 
    qsub code/run_hrt_simulation_150.sh 
    qsub code/run_hrt_simulation_175.sh 
    qsub code/run_hrt_simulation_200.sh 

## Create the figures

Before creating the figures, please ensure that your working directory
is set to symcrt2-manuscript. The figures are placed in the
manuscript/figures directory.

    # Figure 1
    Rscript figures_code/plot_figure_1.R

    # Figures 2,3,4.
    Rscript figures_code/plot_stat_figures.R

    # Choosing the splitting proportion (Figures 5-8 in the Appendix)
    Rscript figures_code/plot_choose_proportions.R

# Real data analysis

## Download real data

Download the `LIU22` directory from here: [Dropbox LIU22
repository](https://www.dropbox.com/scl/fo/qz1ctahx7tn5i2barich9/ABYSILth4DWkAyzqCc9umkU?rlkey=7lp7suuzvd126vdc4jxexynv5&dl=0)
and place the LIU22 directory into LOCAL_EXTERNAL_DATA_DIR. This can
also be done using the following commands:

First,

    source ~/.research_config
    cd $LOCAL_EXTERNAL_DATA_DIR
    wget --max-redirect=20 -O download.zip https://www.dropbox.com/scl/fo/qz1ctahx7tn5i2barich9/ABYSILth4DWkAyzqCc9umkU?rlkey=7lp7suuzvd126vdc4jxexynv5&dl=1

Then, execute

     unzip -o download.zip

Navigate to the symcrt2-manuscript directory. All scripts below must be
executed from this directory.

## Pre-process the data.

    Rscript data_analysis/preprocess.R

## Run the analyses

    # tower PCM
    qsub data_analysis/run_r_script.sh data_analysis/split_pcm_da_manu.R

    # tower GCM
    qsub data_analysis/run_r_script.sh data_analysis/gcm_da_manu.R

    # HRT
    qsub data_analysis/run_r_script.sh data_analysis/hrt_da_manu_035.R

    # PCM
    qsub data_analysis/run_r_script.sh data_analysis/oat_pcm_da_manu_035.R

## Make the results table

    Rscript data_analysis/construct_results_table.R

# Acknowledgments

We thank Timothy Barry and Ziang Niu for sharing their code and
providing inspiration for our code pipeline.
