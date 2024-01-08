
#source ~/.myshrc
cwd=${PWD}
for f in */
do  
        if [ -s $f/OSZICAR ]; then
                string=$( tail -1 $f/OSZICAR ) 
                string=( $string )
                if [ "F=" = ${string[1]} ]; then
                        echo $f ${string[@]}
                else
                        echo $f ${string[@]::5}
                        #cd $cwd/$f
                        #run
                        #cd $cwd
                fi  
        elif [ $1 ]; then
                echo $f
                #cd $cwd/$f
                #run
                #cd $cwd
        fi  
done
echo
#for f in */
#do
#        if [ -s $f/OSZICAR ]; then
#                cd $f
#                string=$( ${HOME}/bin/forces.pl )
#                string=( $string )
#                echo $f ${string[@]:24}
#        fi  
#        cd $cwd
#done 
echo enter any argument to print empty folders
