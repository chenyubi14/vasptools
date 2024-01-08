if [ -z $1 ]; then
	echo 'Error! Enter 1/2 for first/second wannier run'
	exit #return #exit #0
fi  

# check whether it is the first step or the second step
source ~/.myshrc
#if [ -f wannier90.wout ] && [ -s wannier90.wout ];then
if [ $1 = 1 ]; then
	echo 'the first step, run wannier90.x'
	cp $SUBMITWANNIER2 wannier90.job
	echo 'submit.job copied and "run local wannier.job"'
elif [ $1 = 2 ]; then
	echo 'the second step, generate bxsf file'
	file=wannier90.win
	if [ -f OUTCAR ]; then
		string=$(sh $SCRIPT/out_fermi_energy.sh )
		echo 'Read Fermi energy from OUTCAR' $string
	else
		echo 'OUTCAR not found'
		exit
	fi
	sed -i "1s/^/fermi_surface_plot = true\n/" $file
	string="fermi_energy = "${string}"\n"
	sed -i "1s/^/$string/" $file
	sed -i "1s/^/restart = plot\n/" $file
	cp $SUBMITWANNIER2 wannier90.job
	echo 'submit.job copied and "run local wannier90.job"'
else
	echo 'argument not recognized'
fi


