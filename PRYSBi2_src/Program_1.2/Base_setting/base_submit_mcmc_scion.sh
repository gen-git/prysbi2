#!/bin/sh
#PBS -q val_size
#PBS -T sgimpt
#PBS -b val_select
#PBS -l cpunum_job=val_ncpus
#PBS -N Oenumber
#PBS -l memsz_job=60GB
#PBS -o standardo.txt
#PBS -e standarde.txt

cd $PBS_O_WORKDIR
mpirun ${NQSII_MPIOPTS} -np val_ncpus ./main_mcmc_cropcrop.out
