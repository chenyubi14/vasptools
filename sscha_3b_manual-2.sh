#!/usr/bin/env bash

if [ -z $1 ]; then
    echo 'Error! Enter population number (optional: population_direc, run_job_direc)'
    exit
fi

POPULATION=$1
if [ -z $2 ]; then
    DATA_DIR=population$POPULATION
elif [ -d $2 ]; then
    DATA_DIR=$2
else
    echo Error! $2 is not an ensemble population folder
    exit
fi

# Define a directory in which to save all the input files
if [ -z $3 ]; then
    TARGET_DIRECTORY=run_job$POPULATION
else
    TARGET_DIRECTORY=$3
fi

mkdir -p $TARGET_DIRECTORY
cp INCAR KPOINTS POTCAR  $TARGET_DIRECTORY
echo Convert structures in $DATA_DIR/scf_population.dat to $TARGET_DIRECTORY'/*/POSCAR'
python $SCRIPT/sscha_qe2vasp-multi.py $DATA_DIR $TARGET_DIRECTORY

for file in `ls $DATA_DIR/scf_population${POPULATION}*.dat`
do
    # Extract the configuration index
    # (the grep command returns only the expression
    #  that matches the regular expression from the file name)
    #index=`echo $file | grep -oP '(?<=population1_).*(?=\.dat)'`
    index=`echo $file  | grep -oP '(?<=/).*(?=\.dat)' | grep -oP '(?<=population).*(?=)' | grep -oP '(?<=_).*(?=)'`
    echo index=$index

    target_input_dir=$TARGET_DIRECTORY/$index
    mkdir -p $target_input_dir

    ## Convert the file to vasp POSCAR
    #python $SCRIPT/sscha_qe2vasp-single.py $file $target_input_dir/POSCAR

    ## Other input files
    cd $target_input_dir
    ln -s ../INCAR
    ln -s ../KPOINTS
    ln -s ../POTCAR
    ln -s ../POSCAR$index POSCAR
    cd ../../ #$TARGET_DIRECTORY
done

echo VASP jobs generated in $TARGET_DIRECTORY, run with rotate.sh or manage.sh
echo Note POTCAR might need to be regenerated, because POSCAR does not follow the natual order
