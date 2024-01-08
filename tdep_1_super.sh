#!/usr/bin/env bash

if [ -z $1 ] || [ -z $2 ] || [ -z $3 ]; then
    echo -e 'Should enter how to generate a supercell. Three integers.'
    exit 0
fi

if [ -f infile.ucposcar ]; then
    echo 'infile.ucposcar exists, copy to POSCAR'
    cp infile.ucposcar POSCAR
else
    echo 'infile.ucposcar not exists, copy from POSCAR'
    cp POSCAR infile.ucposcar
fi
#cp POSCAR infile.ucposcar

## load modules
module load hdf5/1.10.5
module load mpi 
module load mkl
## use tdep to generate structure
generate_structure -d $@
## generate_structure is in ~/bin/

echo 'SuperPOSCAR to infile.ssposcar and POSCAR'
mv outfile.ssposcar infile.ssposcar
cp infile.ssposcar POSCAR

echo 'Use single k point. ' 
echo 'K-Points
0
Gamma
1 1 1
0 0 0' > KPOINTS
cat KPOINTS
echo 'Before running VASP: update INCAR'
echo -e '\n\t update_edit_incar.py md1 TEBEG'

#python $SCRIPT/update_edit_incar.py md + TEBEG
