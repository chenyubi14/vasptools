#!/usr/bin/env bash

if [ -z $1 ]; then
    echo 'Error! Enter temperature'
    exit
fi
temperature=$1
population=( `ls dyn_end_population* | grep -oP '(?<=).*(?=_)' | sort -u`)
population=${#population[@]}
DATA_DIR=population.hessian
DATA_DIR=$DATA_DIR$population
RUN_DIR=run_job.hessian$population


echo Run below to postprocess vasp data in $RUN_DIR, popu=$population 
echo -e '\t' '(if vasp) sh $SCRIPT/sscha_3c*' $population $DATA_DIR  $RUN_DIR '(final_conf) (init_conf)'
echo -e '\t' '(or mlff) sh $SCRIPT/sscha_3i*' $population $DATA_DIR  $RUN_DIR mlff_job$population #'(final_conf) (init_conf)'
#sh $SCRIPT/sscha_3c* $population $DATA_DIR  $RUN_DIR

echo -e '\t' python '$SCRIPT/sscha_4c*' $temperature $population --direc $DATA_DIR
#python $SCRIPT/sscha_4c* $temperature $population --direc $DATA_DIR
