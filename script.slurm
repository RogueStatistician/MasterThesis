#!/bin/sh
#SBATCH --job-name=MultipleImputation
#SBATCH --account=simonet
#SBATCH --time 100
#  (estimated run time in minutes)
#SBATCH --output=/home/simonet/stdout.log
#SBATCH --error=/home/simonet/stderr.log
#SBATCH --cpus-per-task=10
#  (change this value if you have a multithreaded program eg: --cpus-per-task=4 if you want to use 4 cores, 
#    if you use OpenMP set export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK )
#SBATCH --ntasks=1
#  (default value, use this value if you want to execute a job on multiple nodes (openmpi))
#SBATCH --mem=128
#  (memory in MB)
#SBATCH --partition=normal
#   (use the normal partition=default)
#SBATCH --qos=normal
#   (use the normal qos=default)
cd /path/where/to/start/the/job
module load R

Rscript --vanilla test1.R