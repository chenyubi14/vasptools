#!/bin/bash

# NOTE: don't use 'python $SCRIPT/rotate.sh' to run this code!! Use $SCRIPT/rotate.sh!
# * means all files

################################################
# choose folders with specific pattern
export begin='' # folders should start with $begin string
export middle='' # folders should contain $middle in the middle
export end='' # folders should end with the $end string
export depth='1'
export candidates=*/
################################################


function arg_parse(){
    arg=$1
    if [ ${arg: 0:6} = 'begin=' ];then
        export begin=${arg:6}
    elif [ ${arg: 0:7} = 'middle=' ]; then
        export middle=${arg:7}
    elif [ ${arg: 0:4} = 'end=' ]; then
        export end=${arg:4}
    elif [ ${arg: 0:7} = 'depth=1' ]; then
    	export candidates=*/
	export depth=${arg:6}
    elif [ ${arg: 0:7} = 'depth=2' ]; then
    	export candidates="*/ */*/"
	export depth=${arg:6}
    elif [ ${arg: 0:7} = 'depth=3' ]; then
    	export candidates="*/ */*/ */*/*/"
	export depth=${arg:6}
    fi
}
while [[ $1 = *'='* ]];do
    echo 'parse ' $1
    arg_parse $1
    shift
done

echo -e 'begin='$begin ', middle='$middle ', end='$end ', depth='$depth ' \n'
echo candidates: ${candidates}

if [ $1 ]; then
    # the first argument is usually a .py/.sh file like '${SCRIPT}/op_rerun.sh' #fil='${SCRIPT}/op_eps.py'
    # parse arguments
    fil=$1
    echo 'will execute this ' $fil
    shift
else
	ifexit=1
fi


folders=()
for f in $candidates #*/ */*/ */*/*/
do 
	if [[  $f == ${begin}*${middle}*${end}/ && -d $f  ]] ; then # find filenames like hse*AEXX and is a directory
		folders+=($(pwd)/$f)
		if [ $ifexit ]; then
			echo $f 
		fi
	fi
done
if [ $ifexit ];then	
	echo -e '\nAbove are selected folders. To run: '
    echo -e ' sh $SCRIPT/rotate1* + run + (mode) '
    echo -e ' sh $SCRIPT/rotate1* + $SCRIPT/*.py + (arguments)'
    echo -e ' sh $SCRIPT/rotate1* + $SCRIPT/*.sh +(arguments)'
    echo -e 'other examples: mv $f ${f/=/} \t' ' cp -r $f ${f:0:12}_charge1e${f:21}'
	exit
fi

# loop over each folder
#filefrey='/inter_defect_update_freysoldtcorr.sh'
#length_filefrey=-${#filefrey}
if [[ ${fil} = run || ${fil} = *vasp* || ${fil} == *remote* ]]; then
    source ${HOME}/.myshrc # this contains going back to $WORK
fi
for f in ${folders[@]} ; do
	cd $f
	echo $(pwd)
	#if [[ ${fil} = run || ${fil} = *vasp* ]]; then
		#source ${HOME}/.myshrc # this contains going back to $WORK
		#cd $f
	#	${fil} $@
	#if [ ${fil: $length_filefrey} = ${filefrey} ]; then
	#	${SCRIPT}/${filefrey} 1 $(pwd)/ elec #${f/1e_/0e_}
	#	#echo 1 $(pwd) ${f/1e_/0e_}
	if [ ${fil: -2} = py ];then
		python $fil $@ ##python $SCRIPT/$fil
	elif [ ${fil: -2} = sh ]; then
		sh $fil $@ 
        ## sh ${SCRIPT}/$fil $@
	else
		$fil $@
	fi 
done
