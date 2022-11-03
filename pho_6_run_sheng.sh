
# generate CONTROL
cont=CONTROL


if [ -f band.conf ]; then
	echo 'found band.conf to read DIM'
else
	echo 'band.conf not found. Need it to read DIM, may also enter POSCAR.unit'
	exit 0
fi
dim=$(cat band.conf | sed -n '/DIM/p')
dim=${dim:6} # remove "DIM = " in "DIM = 4 4 4"
echo -e 'the dimension in 2ndorder is ' $dim '\n'

if [ $1 ]; then
	fil=$1
	echo 'unit POSCAR is ' $fil
elif [ -f POSCAR.unit ]; then
	fil=POSCAR.unit
	echo 'found POSCAR.unit. Generate CONTROL...'
else
	echo 'not found POSCAR.unit, should enter the unit file POSCAR. Generate CONTROL...'
	exit 0
fi


if [ -f BORN.ALL ]; then
	echo 'found BORN.ALL, CONTROL will have born'
	python $SCRIPT/pho_5_2_generate_control_NAC.py  $fil $dim
else
	echo 'not found BORN.ALL'
	python $SCRIPT/pho_5_1_generate_control_noNAC.py  $fil $dim
fi


# verify the input files exist
if [ -f FORCE_CONSTANTS_2ND ] && [ -f FORCE_CONSTANTS_3RD ] && [ -f CONTROL ]  ; then
	echo -e '\nForce constants and CONTROL files found. Ready to run ShengBTE'
	echo -e '\nmay need to change ngrid'
else 
	echo -e '\nWait!!! FORCE_CONSTANTS_2ND or FORCE_CONSTANTS_3Ra or CONTROL file not found!!!!'
	echo 'cp ../_2ndorder/FORCE_CONSTANTS  FORCE_CONSTANTS_2ND '
fi

