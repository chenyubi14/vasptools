#!/bin/bash

N_CONFIGS=10
POPULATION=1
PATH_TO_DIR="data/output_espresso"
N_ATOMS=32


ENERGY_FILE="data/energies_supercell_population${POPULATION}.dat"

# Clear the energy file
rm -rf $ENERGY_FILE

for i in `seq 1 10`
do
    filename=${PATH_TO_DIR}/structure_$i.pwo
    force_file=data/forces_population${POPULATION}_$i.dat
    stress_file=data/pressures_population${POPULATION}_$i.dat

    # Get the total energy
    grep ! $filename | awk '{print $5}' >> $ENERGY_FILE
    grep force $filename | grep atom | awk '{print $7, $8, $9}' > $force_file
    grep "total   stress" $filename -A3 | tail -n +2 | awk '{print $1, $2, $3}' > $stress_file
done
