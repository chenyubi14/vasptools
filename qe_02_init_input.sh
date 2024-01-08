#!/usr/bin/env bash

if [[ -z $1 ]]; then
    echo 'Enter prefix!'
    exit
else
    prefix=$1
fi


if [[ -z $2 ]]; then
    fil=in.qe
    echo use the default filename $fil, or enter as '$2'
else
    fil=$2
fi

temp=in.temp
info='&IONS\n!!required for relax, vc-relax, md, vc-md\n/\n&CELL\n!!required for vc-relax, vc-md\n/'

sed -i "s/.*calculation.*/  calculation = 'vc-relax'/" $fil
sed -i '/prefix/d' $fil
sed -i "2i \ \ prefix = '$prefix'" $fil
sed -i "4i \ \ restart_mode = 'from_scratch'" $fil
sed -i "s@.*pseudo_dir.*@  pseudo_dir = '../pseudo/'@" $fil
sed -i '/degauss/d' $fil
sed -i "s/.*smearing =.*/  smearing = 'gauss'/" $fil
sed -i "/.*smearing =.*/a \ \ degauss = 0.01" $fil
sed -i "/ATOMIC_SPECIES/i $info" $fil

echo '(1) UPF pseudopotential filenames in ATOMIC_SPECIES'
echo '(2) edit KPOINTS'
echo '(3) if metal, setup smearing '
echo 'run and get optimal parameters for the current setup'

