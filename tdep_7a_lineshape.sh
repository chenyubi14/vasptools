#!/usr/bin/env bash

module load hdf5/1.10.5
module load mpi 
module load mkl 

mpirun lineshape --path -qg 7 7 7 -ne 600 --temperature 800
# produce outfile.sqe  and outfile.sqe.hdf5
