
### for CONTROL file
if [ -f CONTROL ]; then
    echo 'CONTROL file exists'
else
    ## get DIM from band.conf
    if [ -f band.conf ]; then
        dim=$(cat band.conf | sed -n '/DIM/p')
        dim=${dim:6} # remove "DIM = " in "DIM = 4 4 4"
        echo -e 'Read band.conf to get DIM= ' $dim
    else
        echo 'Not found band.conf. Need it to read DIM. '
        exit 0
    fi
    ## get atom positions from POSCAR
    if [ -f POSCAR.unit ]; then
        fil=POSCAR.unit
        echo 'found POSCAR.unit'
    elif [ -f POSCAR ]; then
        fil=POSCAR
        echo 'found POSCAR'
    else
        echo 'Not found POSCAR.unit'
        exit 0
    fi
    # generate CONTROL file
    if [ -f BORN.ALL ]; then
        echo 'found BORN.ALL, CONTROL will have born. Generating CONTROL...'
        python $SCRIPT/pho_6c_generate_control_NAC  $fil $dim
    else
        echo 'not found BORN.ALL. Generating CONTROL...'
        python $SCRIPT/pho_6a_generate_control_noNAC.py  $fil $dim
    fi
fi

# verify the input files exist
if [ -f FORCE_CONSTANTS_2ND ] && [ -f FORCE_CONSTANTS_3RD ] && [ -f CONTROL ]  ; then
    echo -e 'Input files are found (Force constants and CONTROL). Ready to run ShengBTE (test ngrid)'
else 
	echo -e '\nWait!!! FORCE_CONSTANTS_2ND or FORCE_CONSTANTS_3RD or CONTROL file not found!!!!'
	echo 'cp ../_2ndorder/FORCE_CONSTANTS  FORCE_CONSTANTS_2ND '
fi

if [ $1 ]; then
    defaultfol=$1
else
    defaultfol=sheng1_default
    echo 'generate folder:' $defaultfol ', may enter one argument for a new folder name'
fi
mkdir $defaultfol
cd $defaultfol
ln -s ../FORCE_CONSTANTS_2ND
ln -s ../FORCE_CONSTANTS_3RD
cp ../CONTROL .
echo -e 'COMMENT=\nlegend=' > SAVEINFO
