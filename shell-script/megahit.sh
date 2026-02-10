#!/bin/bash

#SBATCH --job-name=megahit
#SBATCH --output=megahit.out
#SBATCH --error=megahit.err
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=50
#SBATCH --time=0
 
source /share/app/miniconda3/etc/profile.d/conda.sh

conda activate megahit

for i in `tail -n+2 result/metadata.txt|cut -f1`;do
mkdir -p temp/megahit/${i}
megahit -t 50 \
        -1 temp/qc/${i}_1.fastq \
        -2 temp/qc/${i}_2.fastq \
        -o temp/megahit/${i}
done