if [ -d dfpt ] ; then
	echo 'finite displacement files exist. Do you want to remove them? yes(y) for remove'	
	read remove
	if [ ${remove[0]} == y ] || [ ${remove[0]} == Y ] ;then
		rm -r dfpt
		echo 'old files cleaned'
	else
		exit 0
	fi
fi

echo 'Will generate a DFPT folder'

if [ -z $1 ] || [ -z $2 ] || [ -z $3 ]; then
    echo -e 'Should enter how to generate a supercell. Three integers.'
    echo 'Enter one more argument for submitting the jobs: run + local/single'
    exit 0
fi
phonopy -d --dim  $1 $2 $3

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
python $SCRIPT/update_edit_incar.py dfpt

echo remove finite displacement POSCARs
rm POSCAR-*

dfpt='dfpt'
mkdir $dfpt
cp INCAR POTCAR KPOINTS $dfpt
mv SPOSCAR $dfpt/POSCAR


cwd=${PWD}
if [[ $4 && $4 == run ]]; then
    echo 'will run dfpt job'
    source ~/.myshrc
    cd $dfpt
    run $5
fi
cd $cwd
# in INCAR:
## ISIF=2
## ADDGRID=.TRUE. (not essential, but does not cost much either)
## ISYM=0

# Edit KPOINTS
echo 'create band.conf mesh.conf'
atom=$(sed -n '6p' POSCAR)
echo -e 'ATOM_NAME =' $atom '\nDIM =' $1 $2 $3 '\nBAND_POINTS = 101 \nFORCE_CONSTANTS = READ \nFULL_FORCE_CONSTANTS = .TRUE. ' > band.conf
#echo -e 'ATOM_NAME =' $atom '\nDIM =' $1 $2 $3 '\nPRIMITIVE_AXIS = AUTO \nBAND = \nBAND_LABELS = \nBAND_POINTS = 101 \nBAND_CONNECTION = .TRUE. \nFORCE_CONSTANTS = WRITE \nFULL_FORCE_CONSTANTS = .TRUE. \n' > band.conf
echo -e 'ATOM_NAME =' $atom '\nDIM =' $1 $2 $3 '\nMP = 20 20 20' > mesh.conf


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

echo 'run dfpt by an extra argument $4=run or manually run'

python $SCRIPT/inter_check_pho.py
