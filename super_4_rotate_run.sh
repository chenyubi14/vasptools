#!/bin/bash

# NOTE: don't use 'python $SCRIPT/rotate.sh' to run this code!! Use $SCRIPT/rotate.sh!
# * means all files

# run supercell_1,2_*.py  
# 2nd should be the folder that directly saves the supercell files, but now it is the parent folder

function makefoldername() {
	if [ "$1" ]; then
		f1=$1
		if [ ${f1: -1} != / ];then
				f1=${f1}/
		fi  
	else
		echo -e 'Missing arguments. Need folder_from and folder_to \n'
		exit
	fi
	echo $f1
}



folder_from=$(makefoldername $1) # should be 2nd order folders, not 3rd subfolders
folder_to=$(makefoldername $2) #'sources/super_perfect_beo'
echo -e 'from folder' $folder_from ' \n  to folder' $folder_to

#header='v610aexx'
header='v610aexx'
var='_HFSCREEN'

filtype=$3
if [ $filtype == 1 ];then
	fil='supercell_1_generate_inputs.py'
elif [ $filtype == 2 ]; then
	fil='supercell_2_perturb_vacancy_antisite.py'
else
	echo 'Which python file do you want to use? 1,2,3,4?'
	exit
fi
echo 'use this python file' $fil


folders=()
for f in ${folder_from}*
do 
	#echo $f
	if [[  $f == ${folder_from}${header}*${var} && -d $f  ]] ; then # find filenames like hse*AEXX and is a directory
		folders+=($f)
	fi
done


# loop over each folder
for f in ${folders[@]} ; do
	#cd $f
	echo $f
	python ${SCRIPT}/$fil $f $folder_to
done
