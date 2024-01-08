#!/usr/bin/env bash
if [ -z $1 ] ; then
	echo 'Error! Should enter the directory where WAVECAR is saved'
    echo 'Use the WAVECAR from an ISYM=-1 directory without SOC. Also use NBANDS'
	exit 0
fi
sr=$1
ln -s $sr/WAVECAR WAVECAR 
ln -s $sr/CHGCAR CHGCAR
nband=( $(sed -n '6p' $sr/EIGENVAL) )
nband=${nband[-1]}
echo 'the NBANDS will be ' $nband '*2'
python $SCRIPT/update_edit_incar.py nband  $nband
