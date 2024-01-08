
source ~/.myshrc
cwd=${PWD}
for f in dis_*
#for f in dis_{0001..0200}
do  
	cd $cwd/$f
	echo -e '\n' $f
	jobid=( $(cat job.number ) )
	jobid=${jobid[-1]}
	#nostart=$(ls *$jobid*.out)
	nostart=$( ls *.out | grep $jobid )
        if [ $nostart ]; then # job already started
		# check whether job finished
		running=( $( scontrol show job $jobid | grep RUNNING ) ) # running jobs will be not-empty. Stoped jobs will return an empty object
		if [[ -z ${running} ]]; then # not running
			if [ ! -f OSZICAR ]; then
				echo OSZICAR file not exists, directly resubmit
				#cp $cwd/INCAR  $cwd/$f/INCAR 
				#run local
			else
				string=$( grep F= OSZICAR )
				if [[ -z ${string[0]} ]]; then
					echo job stopped, cancel/error
					#echo cp INCAR, resubmit
					cp $cwd/INCAR  $cwd/$f/INCAR 
					cp $cwd/submit.job  $cwd/$f/submit.job 
					run local
				else
					#echo $string
					#echo job finished
					algo=$(grep IALGO OUTCAR | grep 38)
					if [[ $algo ]]; then
						echo ALGO=Normal
						#cp -rd $cwd/$f $cwd/test1_algoNormal
						#cp $cwd/INCAR $cwd/$f/INCAR
						#run local
                    else
                        echo ALGO=Fast !!!!
					fi
					tail -2 *${jobid}*out
				fi
			fi
		else # job still runing, check for error
			occur=$( cat *${jobid}*out | grep 'charge density differ' | wc -l ) # number of occurance for 'charge density differ'
			if [ $occur -gt 5 ]; then
				echo job running, charge density differ $occur times
				scancel $jobid ; echo 'cancel the job'
			else
				echo job running
			fi
			
		fi
	else # job not started yet, update the status
		echo $jobid new job waiting, check newest INCAR
		#diff INCAR ../INCAR
		#echo cp INCAR
		cp $cwd/INCAR  $cwd/$f/INCAR 
	fi
	cd $cwd
done
