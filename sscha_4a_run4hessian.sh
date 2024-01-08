#!/usr/bin/env bash

if [ -z $1 ]; then
    echo 'Error! Enter temperature [hessian configurations]'
    exit
fi

if [ -z $2 ]; then
    echo 'number of hessian configurations is by default 5000'
    num_conf=5000
else
    echo 'number of hessian configurations is now ' $2
    num_conf=$2
fi

temperature=$1
population=( `ls dyn_end_population* | grep -oP '(?<=).*(?=_)' | sort -u`)
population=${#population[@]}
echo found $population populations
population=`expr ${population} + 1 `
echo hessian will be the ${population}th population

DATA_DIR=population.hessian
RUN_DIR=run_job.hessian$population

python $SCRIPT/sscha_3a* $temperature $population --num_conf $num_conf --sobol 1 --direc $DATA_DIR
## sobol=True for less configurations
DATA_DIR=$DATA_DIR$population

bash $SCRIPT/sscha_3b* $population $DATA_DIR  $RUN_DIR

echo run vasp jobs in $RUN_DIR


