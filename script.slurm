#!/bin/bash
#SBATCH --time=01:00:00
#SBATCH --job-name="A long job"
#SBATCH --mem=5GB
#SBATCH --output=long-job.out
cd ~/Documents/MasterThesis

# This may vary per HPC system. At USC's hpc system
# we use: source /usr/usc/R/default/setup.sh
module load R

Rscript --vanilla test1.R