#!/bin/bash
# Goal: generate POTCAR based on POSCAR
# (1) read POSCAR to get the elements 
# (2) generate POTCAR
# It cannot tell which POTCAR to use if there are many POTCAR folders

# -n string: True if the length of "STRING" is non-zero.
if [[ ! -n "$1" ]] ; then
    POTDIR=${HOME}'/repo/pseudopotentials/PBE'
else
    POTDIR=$1
fi 
# -s POSCAR: True if FILE exists and has a size greater than zero.
if [[ ! -s POSCAR ]] ; then 
    echo "ERROR: no POSCAR"
    exit
fi
# procede if POSCAR exist and POSCAR has size > 0
rm -f POTCAR
if [[ ! -f POTCAR.spec ]]; then
    for i in `sed -ne 6p POSCAR` ; do
        FILE=$POTDIR/$i/POTCAR
        if [[ ! -f $FILE ]] ; then
            FILE=$POTDIR/${i}_sv/POTCAR
            if [[ ! -f $FILE ]]; then
                echo "ERROR: could not find $FILE"
                echo "Usage: `basename $0` path-to-POTCAR.atom files (default: ..)"
                exit
            fi
        fi
        echo "adding $FILE"
        cat $FILE >> POTCAR
    done
else
    echo 'found POTCAR.spec, note the last line is separately handled'
    cp POTCAR.spec POTCAR.spec.save
    echo -e '\t' >> POTCAR.spec.save
    cat POTCAR.spec.save | while read ll; do
        echo $ll
        if [[ $ll ]];then
            FILE=$POTDIR/${ll}/POTCAR
            if [[ ! -f $FILE ]]; then
                echo "ERROR: could not find $FILE"
                echo "Usage: `basename $0` path-to-POTCAR.atom files (default: ..)"
                exit
            fi
            echo "adding $FILE"
            cat $FILE >> POTCAR
        fi
    done
    rm POTCAR.spec.save
fi

chmod u+x POTCAR
echo "cat ... >> POTCAR"
