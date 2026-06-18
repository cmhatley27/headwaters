#!/bin/bash
#SBATCH --job-name=rf_cv
#SBATCH --partition=water
#SBATCH --nodes=1
#SBATCH --ntasks=15
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=16g

#SBATCH --output=hw_rf/%x.log
#SBATCH --error=hw_rf/%x.err
#SBATCH --mail-type BEGIN,END
#SBATCH --mail-user camden.hatley@ku.edu
#SBATCH --time=7-00:00:00

export jobname=$SLURM_JOB_NAME
export metrics_sel=$SLURM_JOB_NAME
export cores=$SLURM_NTASKS
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

module load R/4.5
cd $HOME/hw_rf
Rscript scripts/e_RFModels_1_flat_cv.R