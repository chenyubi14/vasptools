#!/usr/bin/env bash

if [ -z $1 ]; then
    echo 'Error! Enter temperature'
    exit
fi
temperature=$1
population=( `ls dyn_end_population* | grep -oP '(?<=).*(?=_)' | sort -u`)
population=${#population[@]}
morepopu=` expr $population + 1 `

if [[ $morepopu -eq 1 ]]; then
    echo ' 
Double check whether you are in the correct directory!
conda activate sscha 

python $SCRIPT/sscha_5a* '$temperature $morepopu' --num_conf 50
sh $SCRIPT/sscha_5b* '$morepopu'
(optional mlff) sh $SCRIPT/sscha_mlff1*' run_job$morepopu mlff_job$morepopu 

else
    echo ' possible commands
(python $SCRIPT/sscha_5a* '$temperature $population') already done
(sh $SCRIPT/sscha_5b* '$population') already done

conda activate sscha 

(if vasp) sh $SCRIPT/sscha_5c* '$population'
(or mlff) sh $SCRIPT/sscha_5i* '$population'
python $SCRIPT/sscha_5d* '$temperature $population' | tee minim_'$population'.out

python $SCRIPT/sscha_5a* '$temperature $morepopu' --num_conf 50
sh $SCRIPT/sscha_5b* '$morepopu'
(optional mlff) sh $SCRIPT/sscha_mlff1*' run_job$morepopu mlff_job$morepopu
fi
