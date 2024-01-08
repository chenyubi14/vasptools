#!/usr/bin/env bash

if [[ -z $1 ]]; then
    echo 'Enter mode! relax or scf '
    exit 0
elif [[ $1 == relax || $1 == 'vc-relax' ]]; then
    mode=1
elif [[ $1 == scf ]]; then
    mode=2
else 
    echo 'Mode not recognized'
    exit 0
fi


## analysis for awk
#awk '/^c/{f=1}     ## If the first character of the line is "c" set f = 1
#  f {$0 = "#" $0}  ## if f is not equal to 0, add "#" to beginning of line
#  {print}          ## print every line
#' file
## another way
#sed '/CELL_PARAMETERS/,$s/^/#/'
fil=in.qe
temp=in.temp

if [[ $mode == 1 ]]; then
    echo 'change cell parameters'
    echo 'Edit (1) ibrav (2) celldm'
    echo '  python $SCRIPT/qe_get_struc.py 4'
    echo '  mv in.intermediate in.qe'
    sed -i "/.*ibrav.*/a  \ \ celldm(1) = " $fil
    awk '/CELL_PARAMETERS/{f=1}f{$0 = "!" $0}{print}' $fil > $temp
    mv $temp $fil
elif [[ $mode == 2 ]]; then
    sed -i "s/.*calculation.*/  calculation = 'scf'/" $fil
    sed -i "/.*ntyp.*/a \ \ nbnd = " $fil
    sed -i "/.*verbosity =.*/a \ \ wf_collect = .true."  $fil
    echo 'Edit (1) nbnd'
fi


