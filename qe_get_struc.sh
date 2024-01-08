#!/usr/bin/env bash

if [[ -z $1 ]];then
    filout=out.qe
else
    filout=$1
fi

if [[ -s $filout ]];then
    echo use QE output file $filout
else
    exit 0
fi


filin=in.qe
filinter=in.intermediate


ibrav=$(awk '/bravais-lattice index/ {print $4}' $filout )
ibrav=( $ibrav )
ibrav=${ibrav[0]}
echo ibrav=$ibrav


python $SCRIPT/qe_get_struc.py  $ibrav --outp $filout --inp $filin --inter $filinter
mv $filinter $filin

echo "unit: angstrom = 1.89 Bohr"
echo "python $SCRIPT/qe_get_struc.py ibrav=4 can also update from ibrav=0 to 4"
