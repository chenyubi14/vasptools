#!/usr/bin/env bash

if [[ -z $1 || -z $2 ]]; then
    echo 'Error! Enter $1=reference_folder and $2=run_folder'
    exit
fi

ref_fol=$1
run_fol=$2
mkdir -p $run_fol
cp $ref_fol/{INCAR,POTCAR,KPOINTS} $run_fol

for fol_i in $ref_fol/*/; do
    fol_i=${fol_i#$ref_fol}
    fol_i=${fol_i#/}
    fol_i=${fol_i%/}
    echo $fol_i
    mkdir -p $run_fol/$fol_i
    cd $run_fol/$fol_i
    ln -s ../{INCAR,ML_FF,POTCAR,KPOINTS} .
    cp ../../$ref_fol/$fol_i/POSCAR .
    cd ../..
done
echo reorder atoms in POSCAR 
python $SCRIPT/sscha_mlff0_sortPOSCAR.py $run_fol
echo INCAR has been updated to use MLFF run
cd $run_fol
python $SCRIPT/update_edit_incar.py mlff2
ln -s ../ML_FF
cd ..
echo "should check 'ln -s ../ML_FF' and 'update POTCAR' because POSCAR is reordered"

