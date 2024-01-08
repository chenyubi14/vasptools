#!/usr/bin/env bash

echo 'phonon calculation for each q point'

fil1=in.1.phonon
fil2=submitq_all.job
fil3=manage.sh

num=$( head harm_dyn0 )
num=($num)
num=${num[3]}

for ind in ` seq 1 $num `; do
    echo $ind
    fil=ph${ind}.in
    sed "/fildvscf/a \ \ last_q = ${ind} " $fil1 | sed "/fildvscf/a \ \ start_q = ${ind}" > $fil
done

#cp $SUBMITQE3 $fil2
sed -e '/q2r.x/d' -e '/matdyn.x/d' $SUBMITQE3 > $fil2
#sed -i '/mpirun/mpirun ph.x -i ph${1}.in > ph${1}.out ' $fil2 


echo -e 'source ~/.myshrc
beg=1
num='$num'
for ind in ` seq $beg $num`; do
    echo $ind
    sed -i "s/.*mpirun.*/echo ph${ind} ;mpirun ph.x -i ph${ind}.in > ph${ind}.out /" '$fil2'
    run local '$fil2' 
done
' > $fil3
chmod u+x $fil3
echo "use $fil3 to submit"
echo "   use less resources in "$fil2 ", but still need large memory"
