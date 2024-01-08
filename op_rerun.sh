#!/bin/sh
# three steps: use CONTCAR, clean files, update INCAR to use WAVECAR

mypwd=${PWD}
source ~/.myshrc
cd $mypwd

function changefiles(){
	num=$1
	cp OUTCAR OUTCAR.$num
	cp INCAR INCAR.$num
	cp vasprun.xml vasprun.xml.$num
    if [ -s CONTCAR ]; then
        cp CONTCAR POSCAR
        echo 'CONTCAR copied'
    else
        echo 'CONTCAR not copied'
    fi
}


if [ -z $1 ];then
	echo 'should enter a number!'
	echo 'will cp OUTCAR/INCAR to *.number'
	echo 'can enter another arguement for (1) using ../INCAR (2) cancel job (3) submit local'
	exit
	#number=$1
else
	echo 'copy OUTCAR, INCAR, vasprun.xml'
fi

changefiles $1

if [ -z $2 ]; then
	echo 'can enter another arguement for (1) using ../INCAR (2) cancel job (3) submit local'
else
	#cp ../INCAR .
	jobid=( $( cat job.number ) )
	jobid=${jobid[-1]}
	found=$( ls pod* | grep $jobid )
	if [ -z $found ]; then
		echo 'not found jobid ' $jobid ', new job waiting'
	else
		echo -e 'found jobid, scancel' $jobid ', submit a new job \n'
		scancel $jobid
		run local
	fi
	#if [[ *{$jobid}* == pod*  ]]; then 
	#	echo new job submited; 
	#else 
	#	echo cancel job $jobid, submit a new job
	#	scancel $jobid
	#	run local
	#fi
fi

