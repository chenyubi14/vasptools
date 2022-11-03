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
echo "find dis* -name vasprun.xml|sort -n|thirdorder_vasp.py reap $1 $2 $3 -$4" > reap.sh
chmod +x reap.sh


echo 3RD.POSCAR.*
total_num=$(ls 3RD.POSCAR.* | wc -l)

if [ -d perfect ]; then
    echo 'perfect/ found, may continue to run'
else
    echo 'perfect/ not found, should have one to copy WAVECAR'
    exit 0
fi

cwd=${PWD}
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
        echo $f in dis_${f: -3}
        mkdir dis_${f: -3} 
        mv $f dis_${f: -3}/POSCAR
        chmod +x dis_${f: -3}/POSCAR
        cp  INCAR POTCAR KPOINTS dis_${f: -3}
    done

    echo 'submit jobs...'
    source ~/.myshrc
    for f in dis_*
    do
        #cp $cwd/perfect/WAVECAR $cwd/$f
        ln -s $cwd/perfect/WAVECAR $cwd/$f/WAVECAR
        cd $cwd/$f
        vasprun $6
    done
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

echo -e 'cwd=${PWD}
source ~/.myshrc
for f in dis_{001..'${total_num}'} 
do  
    #cp $cwd/perfect/WAVECAR $cwd/$f
    #ln -s $cwd/perfect/WAVECAR $cwd/$f/WAVECAR
    #cp $cwd/INCAR $cwd/$f
    cd $cwd/$f
    vasprun
done' > manage.sh
chmod 754 manage.sh

echo -e '
source ~/.myshrc
cwd=${PWD}
for f in dis_{001..'${total_num}'} 
do  
        if [ -s $f/OSZICAR ]; then
                string=$( tail -1 $f/OSZICAR ) 
                string=( $string )
                if [ 'F=' = ${string[1]} ]; then
                        echo $f ${string[@]}
                else
                        echo $f ${string[@]::4}
			#cd $cwd/$f
			#vasprun
			#cd $cwd
                fi  
	elif [ $1 ]; then
		echo $f
		#cd $cwd/$f
		#vasprun
		#cd $cwd
        fi  
done
echo 
echo enter any argument to print empty folders
' > check.sh
chmod 754 check.sh
