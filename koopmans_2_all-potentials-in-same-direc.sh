#!/bin/bash

# Put all vline-eV-a0.dat files in the same new folder

ref='defect'
#ref='bulk'
header='freysoldt'
var='correction_ref-'${ref}

echo $var
# 1-find folders which store vline-eV-a0.dat
folders=()
for f in *
do 
	if [[  $f == ${header}*${var} && -d $f  ]] ; then # find filenames like hse*AEXX and is a directory
		folders+=($(pwd)/$f)
	fi
done


# 2-create a new folder
#newf=sync_freysoldt_ref-defect
newf='sync_freysoldt_ref-'${ref}
if [ -d $newf ]; then
	rm -r $newf
fi
mkdir $newf

# 3-copy files to the new folder
number=$(expr ${#folders[@]} - 1)
echo $number

for i in $( seq 0 $number ); do
	fol=${folders[i]}
	echo $i $newf/${i}vline-eV-a0.dat
	cp $fol/vline-eV-a0.dat $(pwd)/$newf/${i}vline-eV-a0.dat
	cp $fol/vline-eV-a1.dat $(pwd)/$newf/${i}vline-eV-a1.dat
	cp $fol/vline-eV-a2.dat $(pwd)/$newf/${i}vline-eV-a2.dat
	
done
