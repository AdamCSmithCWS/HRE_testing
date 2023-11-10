#!/bin/bash
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=4
#SBATCH --job-name="bbsBayes2_test"

for j in {51..57} 
do
nohup Rscript bbs_script.R 1 $j &> nohup_$j.out &
done




