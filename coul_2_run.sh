#!/usr/bin/env bash
cwd=$PWD
source ~/.myshrc

echo 'assume the current folder has PBE input files'
echo 'Before this, you should edit POSCAR by hand to single out one magnetic atom. '
echo 'Do the entire procedure for every non-identical magnetic atoms'

if [ -z $1 ]; then
    echo -e '\nError! Should enter a string like n d f n, each for one element in POSCAR'
    exit
else
    echo 'The arguments are ' $@
    fin=coul_1_in.sh
    fout=coul_2_out.sh
    echo -e 'rm -r coul_reference/ nsc_* sc_*\nsh $SCRIPT/coul_2*' $@ > $fin
    chmod u+x $fin
    args=( $@ )
    args=( ${args[@]/n} )
    echo -e 'python $SCRIPT/coul_3* ' $args '\n## put atom index starting from zero'> $fout
    chmod u+x $fout
fi

### step 1:
fol1=coul_reference
mkdir $fol1
vaspcp $cwd $fol1
cd $cwd/$fol1 
python $SCRIPT/coul_1_editINCAR.py 0 0.0 $@
touch CHGCAR
touch WAVECAR
touch CONTCAR
cd $cwd


## step 2:
#shifts=(-0.2 -0.18 -0.16 -0.14 -0.12 -0.1 -0.08 -0.06 -0.04 -0.02 -0.0 0.02 0.04 0.06 0.08 0.1 0.12 0.14 0.16 0.18 0.2 )
shifts=(0.0 -0.2 -0.15 -0.1 -0.05 -0.02 0.02 0.05 0.1 0.15 0.2 )
total_num=`expr ${#shifts[@]} - 1 `

for i in $(seq 0 $total_num) 
#for i in {$begin_num..$total_num}
do
    begin_num=00000
    zero_num=`expr ${#total_num} - ${#i} `
    begin_num=${begin_num:1:${zero_num}}
    magn=${shifts[$i]}
    echo $begin_num$i poten_shift=$magn
    ## create folders for each
    fola=sc_$begin_num${i}_coul
    folb=nsc_$begin_num${i}_coul
    mkdir $fola $folb
    ### edit INCAR for each
    ## self-consistent
    cd $cwd/$fola
    cp $cwd/INCAR $cwd/$fola
    ln -sf ../$fol1/CONTCAR POSCAR
    ln -sf ../$fol1/POTCAR
    ln -sf ../$fol1/KPOINTS
    ln -sf ../${fol1}/CHGCAR
    ln -sf ../${fol1}/WAVECAR
    python $SCRIPT/coul_1_editINCAR.py 1 $magn $@ 
    ## non-self-consistent
    cd $cwd/$folb
    cp $cwd/INCAR $cwd/$folb
    ln -sf ../$fol1/CONTCAR POSCAR
    ln -sf ../$fol1/POTCAR
    ln -sf ../$fol1/KPOINTS
    ln -sf ../${fol1}/CHGCAR
    ln -sf ../${fol1}/WAVECAR
    python $SCRIPT/coul_1_editINCAR.py 2 $magn $@
    ## get out of subfolders
    cd $cwd
done



echo -e 'Submit by:
rotate.sh begin=coul run
rotate.sh begin=sc run 
rotate.sh begin=nsc run'
echo 'Remember to run coul_reference/ multiple times, make sure it converges by one ionic step'
