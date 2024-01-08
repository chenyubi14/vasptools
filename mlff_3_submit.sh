#!/usr/bin/env bash

if [[ $1 == run ]];then
    submit=1
    shift
    echo mode argument: $@ '(empty is default)'
else
    submit=0
    echo 'Enter $1=run for submitting'
fi

cwd=$PWD
inter=inter.txt
ls -d *_TEBEG > $inter

fols=($(sort -n $inter) )
echo ${fols[@]} ${#fols[@]}
length=${#fols[@]}
length=`expr $length - 1 `


## for the first job, directly submit
pre=${fols[0]}
cd $pre
if [[ $submit == 1 ]]; then
    source ~/.myshrc
    run $@
fi
if [[ -f job.number ]]; then
    jobid=$(awk '{print $NF}' job.number)
    echo $pre $jobid
fi
cd $cwd

## for the rest, submit with dependency
for j in ` seq 1 ${length}`;do
    current=${fols[$j]} 
    cd $current
    if [[ $submit == 1 ]]; then
        run $@ $jobid
    fi
    if [[ -f job.number ]]; then
        jobid=$(awk '{print $NF}' job.number)
        scontrol show job $jobid | grep Dependency
        echo $current $jobid
    fi
    cd $cwd
    pre=$current
done

rm $inter

