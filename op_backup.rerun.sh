# three steps: use CONTCAR, clean files, update INCAR to use WAVECAR

mypwd=${PWD}
source ~/.myshrc
cd $mypwd

function cleanfiles(){
    # prepare folder files
    if [ -e vasprun.xml ]; then
        echo 'replace POSCAR with CONTCAR, and clean output'
        cp CONTCAR POSCAR
        rm CHG DOSCAR EIGENVAL LOCPOT OSZICAR OUTCAR PCDAT REPORT XDATCAR *.out vasprun.xml
    else
        echo 'The current folder is cleaned already '
    fi
}

function updateincar(){
    python ${SCRIPT}/update_edit_incar.py copy #change3 symprec
    echo 'update INCAR to use WAVECAR, CHGCAR'
}

function rerun() {
    # run folder job
    if [ $1 == default ];then
        run
        #echo 'default'
    else
        run $1 # short/old/unit/large/local
        #echo 'other'
    fi
    echo 'Rerun jobs at the current folder'
}

function vtst_update() {
	# rerun vtst
	for f in *
	do 
		if [[  $f == 0* && -d $f && -s $f/CONTCAR  ]] ; then # find folders
			cp $f/CONTCAR $f/POSCAR
		fi
	done
}

#rm -r dielec_eps
if [ -z $1 ];then
    echo -e "Rerun jobs: use CONTCAR, edit INCAR (selective), clean files. \n\t\$1=default/short/old/unit/large/local, as the argument of run "
    echo -e "\tenter \$2=special for not updating INCAR"
    echo -e "\n\tIf enter neb: will use the local bindary (assumed vtst)"
    echo -e "\n\n For rerunning defect_ jobs, use NUPDOWN!! "
    exit
fi


if [ $1 == neb ];then
	vtst_update
    updateincar
    echo 'submit local (or NEB)'
    run local
    exit
elif [[ $1 == default || $1 == short || $1 == old || $1 == unit || $1 == large || $1 == local ]] ; then
	echo 'Will submit by: run ' $1
    # don't exit, continue to the rest of steps
else
	echo 'argument not recognized'
	exit
fi

#(1): clean files
cleanfiles
#(2): update INCAR to use WAVECAR. 
# only when $2=special, don't want to update INCAR
if [ -z $2 ] || [ $2 != special ];then
    updateincar
else
    echo 'not update INCAR, probably want from scratch again'
fi 
#(3): submit job
rerun $1 # send arguments this way!!

