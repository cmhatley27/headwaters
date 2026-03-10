#!/bin/bash
#SBATCH --job-name=q95_w3_emp
#SBATCH --partition=water
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24
#SBATCH --mem-per-cpu=8gb

#SBATCH --output=hw_rf/%x.log
#SBATCH --error=hw_rf/%x.err
#SBATCH --mail-type BEGIN,END
#SBATCH --mail-user camden.hatley@ku.edu
#SBATCH --time=7-00:00:00

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

export metrics_sel="Q95"
export region_sel="all"
export window_length="3"
export shap_method="empirical"
export model_name="q95_annual_w3_emp"

module load R/4.5
cd $HOME/hw_rf
Rscript e_RFModels_1_annual_vals.R