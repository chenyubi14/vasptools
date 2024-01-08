#!/usr/bin/env bash

temp=$(grep TEBEG= INCAR)
temp=($temp)
temp=${temp#TEBEG=}
echo the temperature is $temp



module load hdf5/1.10.5
module load mpi 
module load mkl

extract_forceconstants -rc2 5 -rc3 3

ln -s outfile.forceconstant_thirdorder infile.forceconstant_thirdorder

