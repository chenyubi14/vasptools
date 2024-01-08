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
if [[ -s INCAR || -s POTCAR || -s KPOINTS || -s POSCAR1 ]]; then
    echo 'Error! Very likely in wrong directory. population/ should not have vasp input files '
    exit
else
    echo 'In the correct directory, will change the order of files'
fi


diff=` expr $newbegin - $begin `
echo difference is $diff
for (( fol=$begin; fol<=$end; fol++ )) 
do
    newfol=` expr $diff + $fol  `
    echo $fol '-->' $newfol
    exist1=$(ls u_population*_$newfol.dat 2> /dev/null  )
    exist2=$(ls scf_population*_$newfol.dat 2> /dev/null  )
    if [[ -z $exist1 && -z $exist2 ]];then
        echo $newfol okay
    else
        echo $newfol exists, double check newbegin
        exit
    fi
    if [ -z $population ]; then
        population=$( ls scf_population*_$fol.dat | awk -F'_' '{print $2}')
        echo $population
    fi
    mv scf_${population}_$fol.dat scf_${population}_$newfol.dat
    mv u_${population}_$fol.dat u_${population}_$newfol.dat
done
