#!/usr/bin/env bash
## load modules
module load hdf5/1.10.5
module load mpi
module load mkl

if [[ $1 ]]; then
    echo 'OUTCAR files will all be used'
    process_outcar_5.3.py $@
else
    echo 'No input of OUTCAR file. Will use ./OUTCAR'
    echo -e '\t Can enter multiple OUTCAR files as arguments!'
    process_outcar_5.3.py OUTCAR
fi

# skip first few steps in OUTCAR
echo Can skip a few steps like 
echo -e '\t python process_outcar_5.3.py OUTCAR* --skip 5 '


## will generate these files
#posfile:     infile.positions
#forcefile:   infile.forces
#statfile:    infile.stat
#metafile:    infile.meta
#latticefile: infile.lattice

if [ -f infile.ucposcar ]; then
    ## extract force constants in ~/bin/
    extract_forceconstants #-rc2 8 -U0 --firstorder 
    ## link forceconstant as infile for phonon calculation
    ln -sf outfile.forceconstant infile.forceconstant
else
    echo 'infile.ucposcar/infile.ssposcar not found, link them to extract 2nd force constants'
fi


## generate infile.lotosplitting file
## extract_forceconstants --polar
