#!/bin/bash
#SBATCH --job-name=run_bar
#SBATCH --time=6:00:00             # Time limit hrs:min:sec
#SBATCH --ntasks=1                  # Run a single task
#SBATCH --output=../results_cluster/out_files/array_%A-%a.out  # Standard output and error log
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=nicolasphilippetodd@protonmail.com
#SBATCH --array=1-80

module purge
module load r/4.0.3

Rscript --vanilla computation.R
