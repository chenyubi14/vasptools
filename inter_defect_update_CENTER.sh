# DEFECT file looks like
#   CENTER=0.50,0.41667,0.56112 # the position of defect center
#   NLINE=58

#Read defect positions from CONTCAR
echo 'Update defect position after relaxation. Assume CONTCAR exists.'
nline=$(cat DEFECT | sed -n '/NLINE/{s/NLINE=//;s/ #.*//;p}' ) # get the number behind NLINE which is line number in vim
newdefect='DEFECT'
nline=($nline)

if [ ${nline[0]} = neighbor ]; then
    #nline = ( $nline )
    #unset $nline[0]
    #nline=(${nline[@]/neighbor}) # 'after unset' ${nline[@]} 
    #echo 'find defect position by averaging its neighbors'
    python ${SCRIPT}/classA_function_defect_average_neighbor_position.py
    #echo 'line' $nline 'DEFECT updated'
else
    newpos=$(nl CONTCAR | sed -n ${nline}p) # read defect position with NLINE
    vecpos=($(echo $newpos | sed -e s/${nline}//g -e 's/T//g' -e 's/F//g') )   # remove nline and remove 'T T T'
    ## because '1 2 3' and   1 2 3   are the same. Make it a list ('1 2 3') can split each one naturally
    #echo ${vecpos[@]}
    # give x,y,z positions individually
    python ${SCRIPT}/classA_function_defect_average_position.py ${vecpos[@]} 
fi
#cat $newdefect
chmod -R 755 DEFECT

# orginal version will omit other arguments in DEFECT, like FREYCORR
#newpos=$(nl CONTCAR | sed -n ${nline}p)
#vecpos=($(echo $newpos | sed -e s/${nline}//g -e 's/T//g' ) )   # remove nline and remove 'T T T'
## because '1 2 3' and   1 2 3   are the same. Make it a list ('1 2 3') can split each one naturally
#printf 'CENTER=%.3f,%.3f,%.3f # the position of defect center\n' ${vecpos[0]} ${vecpos[1]} ${vecpos[2]} > $newdefect
#echo \# $newpos >> $newdefect
#echo NLINE=$nline >> $newdefect
##echo 'line' $nline 'DEFECT updated'