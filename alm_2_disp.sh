#/usr/bin/env bash
folder_name=fin_disp
if [ -d $folder_name ]; then
    echo $folder_name 'exist. Do you want to remove them? yes(y) for remove'
    read remove
    if [ ${remove[0]} == y ] || [ ${remove[0]} == Y ] ;then
        rm -r $folder_name
        echo 'old files cleaned'
    else
        exit 0
    fi
fi
mkdir $folder_name
cd $folder_name
ln -s ../INCAR
ln -s ../KPOINTS
ln -s ../KPOINTS.super
ln -s ../POTCAR
cp ../POSCAR.super.vasp .
mv ../disp*POSCAR .


if [ -d perfect ] || [[ -d dis*/ ]] ; then
	echo 'finite displacement files exist. Do you want to remove them? yes(y) for remove'	
	read remove
	if [ ${remove[0]} == y ] || [ ${remove[0]} == Y ] ;then
		rm -r perfect
		rm -r dis*/
		echo 'old files cleaned'
	else
		exit 0
	fi
fi

echo 'Will generate finite displacement folders'


echo edit KPOINTS...
if [ -f KPOINTS.super ];then
    echo 'KPOINTS.super exists. Will use this file'
    cp KPOINTS.super KPOINTS
    cat KPOINTS
else
    echo -e 'for 2nd order phonon dispersion / number of atoms \n0\nGamma\n1 1 1' > KPOINTS; echo 'using kG111'
fi

echo edit INCAR...
python $SCRIPT/update_edit_incar.py findiff

echo generate dis folders
for f in disp*POSCAR
do 
    echo $f ${f::-7}
    fold=${f::-7}
    mkdir $fold
    mv $f $fold/POSCAR
    cp INCAR POTCAR KPOINTS $fold
done

perfect='perfect'
mkdir $perfect
cp INCAR POTCAR KPOINTS $perfect
cp POSCAR.super.vasp $perfect/POSCAR


cwd=${PWD}
cd $perfect
echo 'edit perfect/INCAR to have WAVECAR'
python $SCRIPT/update_edit_incar.py scf2


cd $cwd

# Edit KPOINTS

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
for f in dis*/ 
do  
    #cp $cwd/INCAR $cwd/$f
    #cp $cwd/submit.job $cwd/$f
    cd $cwd/$f
    ln -sf ../perfect/WAVECAR
    # ln -sf ../perfect/CHGCAR
    run $jobid
done' > manage.sh
chmod +x manage.sh


echo -e '
source ~/.myshrc
cwd=${PWD}
for f in */
do  
        if [ -s $f/OSZICAR ]; then
                string=$( tail -1 $f/OSZICAR ) 
                string=( $string )
                if [ "F=" = ${string[1]} ]; then
                        echo $f ${string[@]}
                else
                        echo $f ${string[@]::5}
                        #cd $cwd/$f
                        #run
                        #cd $cwd
                fi  
        elif [ $1 ]; then
                echo $f
                #cd $cwd/$f
                #run
                #cd $cwd
        fi  
done
echo
for f in */
do
        if [ -s $f/OSZICAR ]; then
                cd $f
                string=$( ${HOME}/bin/forces.pl )
                string=( $string )
                echo $f ${string[@]:24}
        fi  
        cd $cwd
done 
echo enter any argument to print empty folders
' > check.sh
chmod 754 check.sh

echo -e '
title=
legend=
' > SAVEINFO

echo 'Should run perfect first!'

python $SCRIPT/inter_check_pho.py
