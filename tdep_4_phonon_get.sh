#!/usr/bin/env bash

temp=$(grep TEBEG= INCAR)
temp=($temp)
temp=${temp#TEBEG=}
echo the temperature is $temp



fil=infile.qpoints_dispersion
if [ -f $fil ]; then
    echo $fil exists
    cat $fil
else
    echo 'Need infile.qpoints_dispersion!!!!'
    echo 'CUSTOM                  !   
50                      ! Number of points on each path
7                       ! Number paths between special points
0.000 0.000 0.000   0.000 0.000 0.000   $\Gamma$ X 
' > $fil
    exit
fi


module load hdf5/1.10.5
module load mpi 
module load mkl
phonon_dispersion_relations  --temperature $temp -rp --dumpgrid --unit thz

if [ -f SAVEINFO ]; then
    echo 'SAVEINFO' exists
else
    echo "title=
legend=" > SAVEINFO
fi
