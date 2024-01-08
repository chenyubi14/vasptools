if [ -d perfect ] || [ -d dis* ] ; then
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

echo 'Will generate finite displacement folders'

if [ -z $1 ] || [ -z $2 ] || [ -z $3 ]; then
    echo -e 'Should enter how to generate a supercell. Three integers.'
    echo 'Enter one more argument for submitting the jobs: run + local/single'
    exit 0
fi
## use a regular expression to tell whether the argument is an integer
re='^[0-9]+$'
if ! [[ $1 =~ $re ]] ; then
       echo "error: $1 Not a number" >&2; exit 1
   fi
amplitude=0.01
phonopy -d --dim  $1 $2 $3 --amplitude $amplitude

echo edit KPOINTS...
if [ $1 == 1 ] && [ $2 == 1 ] && [ $3 == 1 ];then
    echo  'Super=111. Not using a bigger supercell. Will not use the automatic G111 KPOINTS. Will reuse the current KPOINTS'
    cat KPOINTS
elif [ -f KPOINTS.super ];then
    echo 'KPOINTS.super exists. Will use this file'
    cp KPOINTS.super KPOINTS
    cat KPOINTS
else
    echo -e 'for 2nd order phonon dispersion / number of atoms \n0\nGamma\n1 1 1' > KPOINTS; echo 'using kG111'
fi

echo edit INCAR...
python $SCRIPT/update_edit_incar.py findiff

echo generate dis_ folders
for f in POSCAR-*
do 
    echo $f ${f: -3}
    mkdir dis_${f: -3} 
    mv $f dis_${f: -3}/POSCAR
    cp INCAR POTCAR KPOINTS dis_${f: -3}
done

perfect='perfect'
mkdir $perfect
cp INCAR POTCAR KPOINTS $perfect
mv SPOSCAR $perfect/POSCAR


cwd=${PWD}
cd $perfect
echo 'edit perfect/INCAR to have WAVECAR'
python $SCRIPT/update_edit_incar.py scf2

if [[ $4 && $4 == run ]]; then
    echo 'will run perfect job'
    source ~/.myshrc
    cd $perfect
    run $5
    #for f in dis_*
    #do
    #    cd $cwd/$f
    #    run $5
    #done
fi

cd $cwd
# create dis_00* for each POSCAR-00*
# mv POSCAR-00* to each dis_00* folder, rename POSCAR-00* to POSCAR
# cp INCAR, KPOINTS to each dis_00*
# in INCAR:
## ISIF=2
## NSW=0, IBRION=-1 (don't move ions)
## LREAL = .FALSE. (should not use true)
## ADDGRID=.TRUE. (not essential, but does not cost much either)
## ISYM=0

# Edit KPOINTS
echo 'create band.conf mesh.conf'
atom=$(sed -n '6p' POSCAR)
echo -e 'ATOM_NAME =' $atom '\nDIM =' $1 $2 $3 '\nBAND_POINTS = 101 \nFORCE_CONSTANTS = WRITE \nFULL_FORCE_CONSTANTS = .TRUE. ' > band.conf
#echo -e 'ATOM_NAME =' $atom '\nDIM =' $1 $2 $3 '\nPRIMITIVE_AXIS = AUTO \nBAND = \nBAND_LABELS = \nBAND_POINTS = 101 \nBAND_CONNECTION = .TRUE. \nFORCE_CONSTANTS = WRITE \nFULL_FORCE_CONSTANTS = .TRUE. \n' > band.conf
echo -e 'ATOM_NAME =' $atom '\nDIM =' $1 $2 $3 '\nMP = 20 20 20' > mesh.conf

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
for f in dis_{001..'${f: -3}'} 
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
for f in dis_{001..'${f: -3}'} 
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
for f in dis_{001..'${f: -3}'}
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

length_zero=` expr ${#ZEROFOL} + 1 ` # length of zero folder name counting with characters
cwd=$( echo ${PWD[@]:${length_zero}} ) # the current folder without ZEROFOL, like 'berylliumO/wurtzite_91_defect_complex_VO_LiBe/defect0e'
# length of zero folder, count words, like dength_zero2=4 through counting (work2 07884 tg871834 stampede2)
length_zero2=( $( echo $ZEROFOL | tr  '/', ' ' ) ) # the list (work2 07884 tg871834 stampede2)
length_zero2=` expr ${#length_zero2[@]} + 1 `
FIRSTFOL=( $( echo $PWD | tr  '/', ' ' ) ) # a list with separated folder name, like (work2 07884 tg871834 stampede2 berylliumO wurtzite_91_defect_complex_VO_LiBe defect0e)
FIRSTFOL=/$( echo ${FIRSTFOL[@]::${length_zero2}} | tr ' ' '/' ) # get /work2/07884/tg871834/stampede2/berylliumO
cat $FIRSTFOL/phonopy.conf | grep 'BAND' >> band.conf

echo 'Should run perfect first!'
echo 'amplitude='$amplitude

python $SCRIPT/inter_check_pho.py
