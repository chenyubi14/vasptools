#!/usr/bin/env bash

if [ -z $1 ]; then
    echo 'Error! Enter population number (optional: population_direc, run_job_direc)'
    exit
fi

POPULATION=$1
if [ -z $2 ]; then
    ENSEMDIR=population$POPULATION
elif [ -d $2 ]; then
    ENSEMDIR=$2
else
    echo Error! $2 is not an ensemble population folder
    exit
fi

# Define a directory in which to save all the input files
if [ -z $3 ]; then
    JOBDIR=run_job$POPULATION
else
    JOBDIR=$3
fi


ENERGY_FILE="${ENSEMDIR}/energies_supercell_population${POPULATION}.dat"

# Clear the energy file
rm -rf $ENERGY_FILE

num_fol=`find $JOBDIR -type d | wc -l`
## Remove the extra counting of './'
num_fol=`expr $num_fol - 1 `
echo In total, $num_fol configurations

for dir in `seq 1 $num_fol`
do
    echo $dir
    fol_i=${JOBDIR}/${dir}
    #filename=${fol_i}/structure_$i.pwo
    force_file=${ENSEMDIR}/forces_population${POPULATION}_$dir.dat
    stress_file=${ENSEMDIR}/pressures_population${POPULATION}_$dir.dat

    # Get the total energy
    grep F= $fol_i/OSZICAR | awk '{print $3}' >> $ENERGY_FILE
    sed -e '1,/forces/d' -e '/varray/,$d' $fol_i/vasprun.xml | awk '{print $2, $3, $4}' > $force_file
    #sed -e '1,/TOTAL-FORCE/d' -e '/total drift/,$d' $fol_i/OUTCAR | tail -n +2 | head -n -1 | awk '{print $4, $5, $6}'> $force_file
    sed -e '1,/stress/d' -e '/varray/,$d' $fol_i/vasprun.xml | awk '{print $2, $3, $4}' > $stress_file
     
    ## Unit of force is eV/Angst
    ## force Convert units  eV/Angst -> Ry/Bohr
    python $SCRIPT/sscha_units-force.py $force_file

    ## Unit of stress is kB
    ## stress Convert units  kB -> Ry/Bohr^3
    python $SCRIPT/sscha_units-stress.py $stress_file
done


## Unit of energy is eV
## Energy Convert units eV -> Ry
python $SCRIPT/sscha_units-energy.py $ENERGY_FILE


### Get forces for all atoms
## Find the text between 'TOTAL-FORCE' and 'total drift'
#awk '/TOTAL-FORCE/{flag=1;next}/total drift/{flag=0}flag' OUTCAR
#sed -e '1,/TOTAL-FORCE/d' -e '/total drift/,$d' OUTCAR
# tail -n +2: remove the first line
#  head -n -1 : remove the last line
# awk '{print $4, $5, $6}' ; extract the 4th 5th 6th column
