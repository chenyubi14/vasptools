#!/usr/bin/env bash

if [[ -z $1 || -z $2  || -z $3 ]]; then
    echo 'Enter begin=$1, end=$2, newbegin=$3'
    echo 'Will change {10..20} to {newbegin..newbegin+10}'
    exit
else
    begin=$1
    end=$2
    newbegin=$3
    if ! [[ $begin =~ ^[0-9]+$ ]]; then
        echo $begin is not integer
        exit
    fi
    if ! [[ $end =~ ^[0-9]+$ ]]; then
        echo $end is not integer
        exit
    fi
    if ! [[ $newbegin =~ ^[0-9]+$ ]]; then
        echo $newbegin is not integer
        exit
    fi
    echo begin=$begin end=$end newbegin=$newbegin
fi

# check whether in the correct directory
if [[ -s INCAR && -s POTCAR && -s KPOINTS  ]]; then
    echo 'In the correct directory, will change the order of files'
else
    echo 'Error! Should be in run_job directory, change order here only'
    exit
fi

diff=` expr $newbegin - $begin `
echo difference is $diff
for (( fol=$begin; fol<=$end; fol++ )) 
do
    newfol=` expr $diff + $fol  `
    echo $fol '-->' $newfol
    if [ -s POSCAR$newfol ];then
        echo POSCAR$newfol exists, double check newbegin
        exit
    fi
    mv $fol $newfol
    mv POSCAR$fol POSCAR$newfol
    cd $newfol
    ln -sf ../POSCAR$newfol POSCAR
    cd ..
done
