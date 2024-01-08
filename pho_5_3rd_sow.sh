if [ -d dis* ]  ; then
	echo 'finite displacement files exist. Do you want to remove them? yes(y) for remove'	
	read remove
	if [ ${remove[0]} == y ] || [ ${remove[0]} == Y ] ;then
		#rm -r perfect
		rm -rf dis*_*
		echo 'old files cleaned'
	else
		exit 0
	fi
fi

if [ -z $1 ] || [ -z $2 ] || [ -z $3 ] || [ -z $4 ] ; then
    echo -e 'Should enter how to generate a supercell, and nearest atom number. 3+1 integers. The last integer should not have dash -'
    echo 'Enter one more argument for submitting the jobs: run + local/single'
    exit 0
fi
thirdorder_vasp.py sow $1 $2 $3 -$4
echo "thirdorder_vasp.py sow $1 $2 $3 -$4" > sow.sh
#echo "find dis* -name vasprun.xml|sort -n|thirdorder_vasp.py reap $1 $2 $3 -$4" > reap.sh
echo "ls dis*/vasprun.xml|sort -n|thirdorder_vasp.py reap $1 $2 $3 -$4" > reap.sh
chmod 755 reap.sh


echo 3RD.POSCAR.*
total_num=$(ls 3RD.POSCAR.* | wc -l)

if [ -d perfect ]; then
    echo 'perfect/ found, may continue to run'
else
    echo 'perfect/ not found, should have one to copy WAVECAR'
    #exit 0
fi

cwd=${PWD}
echo 'the fifth argument is ' $5
if [[ $5 && $5 == run ]]; then
    echo edit INCAR...
    python $SCRIPT/update_edit_incar.py findiff
    #echo edit KPOINTS...
    #echo -e 'pymatgen with grid density = 1353 / number of atoms \n0\nGamma\n1 1 1' > KPOINTS
    echo 'reuse KPOINTS'
    cat KPOINTS

    echo 'generate job folders...'
    for f in 3RD.POSCAR.*
    do 
        fold=dis_${f: 11}
        echo $f in $fold
        mkdir $fold 
        mv $f $fold/POSCAR
        chmod +755 $fold/POSCAR
        cp  INCAR POTCAR KPOINTS $fold
    done

    #echo 'submit jobs...'
    #source ~/.myshrc
    #for f in dis_*
    #do
    #    #cp $cwd/perfect/WAVECAR $cwd/$f
    #    ln -s $cwd/perfect/WAVECAR $cwd/$f/WAVECAR
    #    cd $cwd/$f
    #    run $6
    #done
fi

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
echo -e 'ATOM_NAME =' $atom '\nDIM =' $1 $2 $3 'BAND_POINTS = 101 ' > band.conf

begin_num=00000
zero_num=`expr ${#total_num} - 1 `
begin_num=${begin_num:1:${zero_num}} # this many of zeros if zero_num=2, begin_num=00
begin_num=${begin_num}1 # 01 or 001
echo -e 'cwd=${PWD}
source ~/.myshrc
for f in dis_{'${begin_num}'..'${total_num}'} 
do  
    #cp $cwd/INCAR $cwd/$f
    #cp $cwd/submit.job $cwd/$f
    cd $cwd/$f
    ln -s ../perfect/WAVECAR 
    run
    cd $cwd
done' > manage.sh
chmod 754 manage.sh

echo -e '
source ~/.myshrc
cwd=${PWD}
for f in dis_{'${begin_num}'..'${total_num}'} 
do  
        if [ -s $f/OSZICAR ]; then
                string=$( tail -1 $f/OSZICAR ) 
                string=( $string )
                if [ 'F=' = ${string[1]} ]; then
                        echo $f ${string[@]}
                else
                        echo $f ${string[@]::4}
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
for f in dis_{'${begin_num}'..'${total_num}'}
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
