#echo 'At directory ' $PWD
# number of slices
slice_num=150
#slice_num=80
# starting phi
phi1=55
# starting theta
theta1=45
# ending phi
phi2=235
# ending theta
theta2=45
# number of rotation angles
#rotation_num=37
rotation_num=10

#if [ -z $1 ] || [ -z $2 ] || [ -z $3 ]  || [ -z $4 ] ; then
if [ -z $1 ]  ; then
	echo 'Should set $1=phi1, $2=theta1, $3=phi2, $4=theta2, or enter $1=default'
	exit 0
elif [ $1 = default ]; then
	echo -e 'not setting initial angles, will use ' 'phi1=' $phi1 ', theta1=' $theta1 ', phi2=' $phi2 ', theta2=' $theta2 
elif [ $5 ]; then
	rotation_num=$5
	phi1=$1
	theta1=$2
	phi2=$3
	theta2=$4
	#echo 'phi1=' $phi1 ', theta1=' $theta1 ', phi2=' $phi2 ', theta2=' $theta2 ,  'rotation_num=' $5
else
	phi1=$1
	theta1=$2
	phi2=$3
	theta2=$4
fi

# default setup
# file name, find the only bxsf file
#echo 'assume only one bxsf file is in the current directory'
filename=$( ls *.bxsf )
# fermi energy in bxsf file
fermi=$( sed -n '/Fermi Energy:/p' $filename )
fermi=( $fermi)
fermi=${fermi[-1]}
mini_freq=0.000
maxi_frac_diff=0.010
maxi_orb_dist=0.050
wall_orbit=n # Allow extremal orbits near super-cell walls? n for no, y for yes

if [ $rotation_num = 1 ]; then
	rotation_mode='n'
else
	rotation_mode='r'
fi


config=config.in
rm -f $config
printf '%-50s\n' "${filename}"  >> $config
echo -e '' ${fermi} '\n' $slice_num >> $config
echo -e '' $theta1 '\n' $phi1 '\n'${rotation_mode} >> $config
echo -e '' $mini_freq '\n' $maxi_frac_diff '\n' $maxi_orb_dist  '\n'$wall_orbit >> $config
echo -e '' $theta1 '\n' $theta2 '\n' $phi1 '\n' $phi2 '\n' $rotation_num >> $config

#cat $config


#${HOME}/bin/skeaf -rdcfg -nodos 
#other tags: 
## -noext: skip extremal area finding
## -fbxsf filename.bxsf, filename.bxsf will be used 
