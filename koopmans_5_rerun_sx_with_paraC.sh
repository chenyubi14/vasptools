#!/bin/bash

# NOTE: don't use 'python $SCRIPT/rotate.sh' to run this code!! Use $SCRIPT/rotate.sh!
# * means all files

ref='defect'
#ref='bulk'
header='freysoldt'
var='_correction_ref-'$ref


folders=()
for f in *
do 
	if [[  $f == ${header}*${var} && -d $f  ]] ; then # find filenames like hse*AEXX and is a directory
		folders+=($(pwd)/$f)
	fi
done


# loop over each folder
for f in ${folders[@]} ; do
	cd $f
	echo $(pwd)
	sh sx.sh
done
