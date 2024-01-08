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

num_fol=`find $JOBDIR -type d | wc -l`
## Remove the extra counting of './'
num_fol=`expr $num_fol - 1 `
echo Found, $num_fol configurations in total


## all folders: `seq 1 $num_fol`
## However, I want to process some of the folders in advance
## process `seq $begin $final`
## final=$4, begin=$5
## by default final=$num_fol, begin=1

if [ -z $5 ]; then
    begin=1
else
    begin=$5
    re='^[0-9]+$'
    if ! [[ $begin =~ $re ]] ; then
       echo error: begin_conf $begin is not a number >&2; 
       exit
    fi
fi


if [ -z $4 ]; then
    final=$num_fol
else
    final=$4
    re='^[0-9]+$'
    if ! [[ $final =~ $re ]] ; then
       echo error: final_conf $final is not a number >&2; exit 1
    fi
    if [[ $final -gt $num_fol || $begin -gt $final ]] ; then
       echo error: final_conf $final is larger than total configs $num_conf >&2; 
       exit
    fi
fi


echo processed arguments popu=$POPULATION data=$ENSEMDIR job=$JOBDIR processing $begin-$final 


## Only when all energy files are processed, store the energy file
if [[ $begin == 1 && $final == $num_fol  ]]; then
    echo 'All files ready, process energy data'
    sh $SCRIPT/sscha_popu_energy.sh  $POPULATION $ENSEMDIR $JOBDIR $num_fol
fi

## judge whether folder has been processed, if yes, skip it
sh $SCRIPT/sscha_popu_force.sh $POPULATION $ENSEMDIR $JOBDIR $final $begin

