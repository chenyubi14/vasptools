
# Run python formation_energy.py, save the output to graphdata/.

# (1) Enter arguments for the formation_energy.py file
if [ -z $1 ] ;then # if no argument
	echo 'Enter argument, which formation energy file to use'
	ls ~/bin/energyf*py
	exit
fi

echo 'Running...'
python $1 > energyf_output.txt
cat energyf_output.txt
string=$( sed -n '/figure /p' energyf_output.txt ) # get the last line
string=( ${string} ) # a list: ( figure ${HOME}/././.pdf is produced )
string=( $( echo ${string[1]} | tr '/', ' ' ) ) # trim ${HOME}/././.pdf by '/'
string=graphdata/${string[-1]}.txt # the file that saves information
cp energyf_output.txt $string # get the last one, the name of pdf
echo 'data is saved in ' $string
