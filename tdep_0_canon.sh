#!/usr/bin/env bash

if [ -z $1 ] || [ -z $2 ] ; then
    echo 'Enter number of POSCARs, and temperature'
    exit
else
    num_conf=$1
    temperature=$2
fi

cwd=$PWD
echo 'Run after tdep_1_super.sh'
if [[ -d perfect ]] || [[ -d dis* ]] ; then
	echo 'finite displacement files exist. Do you want to remove them? yes(y) for remove'	
	read remove
	if [ ${remove[0]} == y ] || [ ${remove[0]} == Y ] ;then
		rm -r perfect
		rm -r dis*_*
		echo 'old files cleaned'
	else
		exit 0
	fi
fi



### generate cannonical configurations
## load modules
module load hdf5/1.10.5
module load mpi 
module load mkl
canonical_configuration -n $num_conf -t $temperature  --debye_temperature $temperature
python $SCRIPT/update_edit_incar.py md $temperature

## make perfect folders
perfect='perfect'
mkdir $perfect
cp INCAR POTCAR KPOINTS $perfect
cp POSCAR $perfect/POSCAR
cd perfect/
python $SCRIPT/update_edit_incar.py nomd
cd $cwd

echo generate dis_ folders
for f in contcar_conf*
do 
    echo $f ${f: -4}
    mkdir dis_${f: -4} 
    mv $f dis_${f: -4}/POSCAR
    cp INCAR POTCAR KPOINTS dis_${f: -4}
done

echo '
cwd=${PWD}
source ~/.myshrc
if [ -s perfect/WAVECAR ]; then
	echo WAVECAR exist and is not empty, submit without dependence
else
	touch perfect/WAVECAR
	jobid=( $(cat perfect/job.number ) )
	if [[ -z $jobid ]];then
		echo perfect not submitted
		cd perfect/
		run
		echo -e "\n submit perfect, excutate manage.sh again to submit dis_ jobs"
		exit
	else
		jobid=${jobid[-1]}
		echo -e perfect jobid is $jobid, submit jobs... "\n"
	fi

fi
for f in dis_{0001..'${f: -4}'} 
do  
    #cp $cwd/INCAR $cwd/$f
    #cp $cwd/submit.job $cwd/$f
    cd $cwd/$f
    ln -sf ../perfect/WAVECAR
    # ln -sf ../perfect/CHGCAR
    run $jobid
done' > manage.sh
chmod +x manage.sh

python $SCRIPT/inter_check_pho.py
