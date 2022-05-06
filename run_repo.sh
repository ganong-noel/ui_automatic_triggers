#!/bin/bash
#SBATCH --job-name=ui_automatic_triggers
#SBATCH --account=pi-ganong
#SBATCH --time=28:00:00
#SBATCH --partition=caslake
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mem=32G

module load R

Rscript --quiet --no-restore --no-save analysis/source/master.R
