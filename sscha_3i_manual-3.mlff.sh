#!/usr/bin/env bash

if [ -z $1 ]; then
    echo 'Error! Enter $1=population (optional: $2=population_direc, $3=run_job_direc, $4=mlff_job_direc)'
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

if [ -z $3 ]; then
    JOBDIR=run_job$POPULATION
elif [ -d $3 ]; then
    JOBDIR=$3
else
    echo "Error! $3 is not a folder"
fi


if [ -z $4 ]; then
    MLFFDIR=mlff_job$POPULATION
elif [ -d $4 ]; then
    MLFFDIR=$4
else
    echo "Error! $4 is not a folder"
fi


ENERGY_FILE="${ENSEMDIR}/energies_supercell_population${POPULATION}.dat"
# Clear the energy file
rm -rf $ENERGY_FILE

num_fol=`find $MLFFDIR -type d | wc -l`
## Remove the extra counting of './'
num_fol=`expr $num_fol - 1 `
echo In total, $num_fol configurations


## get force and stress in $MLFFDIR
echo process force in $MLFFDIR
sh $SCRIPT/sscha_mlff2_process-force.sh $MLFFDIR
## reorder force and copy to $JOBDIR
python $SCRIPT/sscha_popu_mlff_reorder.py $JOBDIR $MLFFDIR

#exit

echo data for sscha in $ENSEMDIR
for dir in `seq 1 $num_fol`
do
    echo $dir
    fol_i=${MLFFDIR}/${dir}
    #filename=${fol_i}/structure_$i.pwo
    force_file_raw=${JOBDIR}/${dir}/force_file
    stress_file_raw=${JOBDIR}/${dir}/stress_file
    force_file=${ENSEMDIR}/forces_population${POPULATION}_$dir.dat
    stress_file=${ENSEMDIR}/pressures_population${POPULATION}_$dir.dat

    # Get the total energy
    grep F= $fol_i/OSZICAR | awk '{print $3}' >> $ENERGY_FILE
    cp $force_file_raw $force_file
    cp $stress_file_raw $stress_file
     
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
