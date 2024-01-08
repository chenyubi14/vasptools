#!/usr/bin/env bash

POPULATION=$1
ENSEMDIR=$2
JOBDIR=$3
num_fol=$4

ENERGY_FILE="${ENSEMDIR}/energies_supercell_population${POPULATION}.dat"
# Clear the energy file
rm -rf $ENERGY_FILE


for dir in `seq 1 $num_fol`
do
    fol_i=${JOBDIR}/${dir}
    # Get the total energy
    energy=$(grep F= $fol_i/OSZICAR | awk '{print $3}')
    echo $energy >> $ENERGY_FILE
    echo $dir  energy=$energy
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
