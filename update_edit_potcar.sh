#!/bin/bash
# Goal: generate POTCAR based on POSCAR
# (1) read POSCAR to get the elements 
# (2) generate POTCAR
# It cannot tell which POTCAR to use if there are many POTCAR folders

# -n string: True if the length of "STRING" is non-zero.
if [[ ! -n "$1" ]] ; then
    POTDIR=${HOME}'/repo/peudopotentials/PBE'
else
    POTDIR=$1
fi 
# -s POSCAR: True if FILE exists and has a size greater than zero.
if [[ ! -s POSCAR ]] ; then 
    echo "ERROR: no POSCAR"
    exit
fi
# procede if POSCAR exist and POSCAR has size > 0
for i in `sed -ne 6p POSCAR` ; do
    FILE=$POTDIR/$i/POTCAR
    if [[ ! $FILE ]] ; then
	echo "ERROR: could not find $FILE"
	echo "Usage: `basename $0` path-to-POTCAR.atom files (default: ..)"
	exit
    fi
done

rm -f POTCAR
for i in `sed -ne 6p POSCAR` ; do
    FILE=$POTDIR/$i/POTCAR
    echo "adding $FILE"
    cat $FILE >> POTCAR
done
chmod +x POTCAR
echo "done writing new POTCAR"
