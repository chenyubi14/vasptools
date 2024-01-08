#!/usr/bin/env bash

temp=$(grep TEBEG= INCAR)
temp=($temp)
temp=${temp#TEBEG=}
echo the temperature is $temp





module load hdf5/1.10.5
module load mpi 
module load mkl

#mpirun thermal_conductivity -qg 10 10 10 --temperature $temp
thermal_conductivity -qg 10 10 10 --temperature $temp
