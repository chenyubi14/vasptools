#echo wurtzite_10_Vo_findAEXX/relax* | xargs -n 1 cp -v wurtzite_10_Vo_findAEXX/DEFECT
#echo wurtzite_11_Vo_defect/relax* | xargs -n 1 cp -v wurtzite_11_Vo_defect/DEFECT 
#echo wurtzite_12_Vo_findAEXX.largercell/relax* | xargs -n 1 cp -v wurtzite_12_Vo_findAEXX.largercell/DEFECT
#echo wurtzite_21_Vbe_defect/relax* | xargs -n 1 cp -v wurtzite_21_Vbe_defect/DEFECT
#echo wurtzite_31_antio/relax* | xargs -n 1 cp -v wurtzite_31_antio/DEFECT 
#echo wurtzite_41_antibe/relax* | xargs -n 1 cp -v wurtzite_41_antibe/DEFECT 
#echo wurtzite_51_io/relax* | xargs -n 1 cp -v wurtzite_51_io/DEFECT 
#echo wurtzite_61_ibe/relax* | xargs -n 1 cp -v wurtzite_61_ibe/DEFECT 
#echo wurtzite_70_iH_H-Be_bond_findAEXX/relax* | xargs -n 1 cp -v wurtzite_70_iH_H-Be_bond_findAEXX/DEFECT
#array=(wurtzite_10_Vo_findAEXX wurtzite_11_Vo_defect wurtzite_12_Vo_findAEXX.largercell wurtzite_21_Vbe_defect wurtzite_31_antio wurtzite_41_antibe wurtzite_51_io wurtzite_61_ibe wurtzite_70_iH_H-Be_bond_findAEXX )

if [ $1 ]; then
	fil=$1 #'${SCRIPT}/op_rerun.sh' #fil='${SCRIPT}/op_eps.py'
	echo $fil
	shift
else
	echo -e ' Only print selected folders. \nTo run: $1=anything'
	ifexit=1
fi

header='koopmansAEXX';middle='1e_';var='HFSCREEN' # fixed koopmans
#header='koopmans';middle='1e';var='_AEXX' # fixed koopmans
#header='v610aexx';middle='';var='_HFSCREEN'


folders=()
for f in *
do 
	if [[  $f == ${header}*${middle}*${var} && -d $f  ]] ; then # find filenames like hse*AEXX and is a directory
		folders+=($(pwd)/$f)
		if [ $ifexit ]; then
			echo $f #${f/1e_/0e_}
			#cp -r $f ${f:0:12}_charge1e${f:21}
		fi
	fi
done
if [ $ifexit ];then	
	exit
fi

for fol in ${folders[@]}
do 
    cp DEFECT $fol
#echo $fol/relax* | xargs -n 1 cp -v $fol/DEFECT
done
