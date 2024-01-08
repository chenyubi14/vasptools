#!/usr/bin/env bash

if [[ -z $1 ]]; then
    run_fol='./'
    echo 'Can enter $1=run_folder'
else
    run_fol=$1
fi
cdw=$PWD
force_file=force_file
stress_file=stress_file

for fol_i in $run_fol/*/; do
    fol_i=${fol_i#$run_fol}
    fol_i=${fol_i#/}
    fol_i=${fol_i%/}
    echo $fol_i
    cd $run_fol/$fol_i
    sed -e '1,/forces/d' -e '/varray/,$d' vasprun.xml | awk '{print $2, $3, $4}' > $force_file
    sed -e '1,/stress/d' -e '/varray/,$d' vasprun.xml | awk '{print $2, $3, $4}' > $stress_file
    cd $cdw
done
echo processed forces to the file force_file
