#!/usr/bin/env bash

POPULATION=$1
ENSEMDIR=$2
JOBDIR=$3
final=$4
begin=$5


for dir in ` seq $begin $final `
do
    fol_i=${JOBDIR}/${dir}
    force_file=${ENSEMDIR}/forces_population${POPULATION}_$dir.dat
    stress_file=${ENSEMDIR}/pressures_population${POPULATION}_$dir.dat

    if [ -s $force_file ]; then
        echo $dir force is update to date
    else
        echo $dir force updating

        # Get the total energy
        sed -e '1,/forces/d' -e '/varray/,$d' $fol_i/vasprun.xml | awk '{print $2, $3, $4}' > $force_file
        #sed -e '1,/TOTAL-FORCE/d' -e '/total drift/,$d' $fol_i/OUTCAR | tail -n +2 | head -n -1 | awk '{print $4, $5, $6}'> $force_file
        sed -e '1,/stress/d' -e '/varray/,$d' $fol_i/vasprun.xml | awk '{print $2, $3, $4}' > $stress_file
         
        ## Unit of force is eV/Angst
        ## force Convert units  eV/Angst -> Ry/Bohr
        python $SCRIPT/sscha_units-force.py $force_file

        ## Unit of stress is kB
        ## stress Convert units  kB -> Ry/Bohr^3
        python $SCRIPT/sscha_units-stress.py $stress_file
    fi
done


### Get forces for all atoms
## Find the text between 'TOTAL-FORCE' and 'total drift'
#awk '/TOTAL-FORCE/{flag=1;next}/total drift/{flag=0}flag' OUTCAR
#sed -e '1,/TOTAL-FORCE/d' -e '/total drift/,$d' OUTCAR
# tail -n +2: remove the first line
#  head -n -1 : remove the last line
# awk '{print $4, $5, $6}' ; extract the 4th 5th 6th column
