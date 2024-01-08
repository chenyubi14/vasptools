#!/bin/bash

echo 'Append strcutures in scf_population.dat to the input header file espresso_header.pwi'

HEADER_FILE=espresso_header.pwi
DATA_DIR=data
POPULATION=1

# Define a directory in which to save all the input files
TARGET_DIRECTORY=$DATA_DIR/input_files_population$POPULATION

mkdir -p $TARGET_DIRECTORY

for file in `ls $DATA_DIR/scf_population${POPULATION}*.dat`
do
    # Extract the configuration index
    # (the grep command returns only the expression
    #  that matches the regular expression from the file name)
    index=`echo $file | grep -oP '(?<=population1_).*(?=\.dat)'`
    echo index= $index

    target_input_file=$TARGET_DIRECTORY/structure_${index}.pwi

    # Copy the template header file
    cp $HEADER_FILE $target_input_file

    # Attach after the header the structure
    cat $file >> $target_input_file
done

