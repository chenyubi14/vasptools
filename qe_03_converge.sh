#!/usr/bin/env bash

if [[ -z $1 ]];then
    echo 'Enter what tag to test, the rest of arguments should be values'
    exit 0
else
    mode=$1
fi

echo 'convergence test w.r.t ecutwfc/ecutrho, KPOINTS, '
input=in.qe


if [[ $mode == k || $mode == kp || $mode == kpoint || $mode == 1 ]];then
    echo 'kpoint convergence'
    mode=1
    fol=conv1_kpoint
    tag='K_POINTS automatic'
    kratio=$(python $SCRIPT/qe_get_k-ratio.py --inqe $input ) 
    kratio=($kratio)
    ########################################
    #echo 'Edit here to directly use a different kpoint ratio'
    #kratio=( 3 3 2 )
    ########################################
    echo kmesh ratio: ${kratio[@]}
elif [[ $mode == cut || $mode == encut || $mode == ecutwfc || $mode == 2 ]];then
    echo 'energy cutoff convergence'
    mode=2
    fol=conv2_encut
    tag='ecutwfc'
elif [[ $mode == density || $mode == ecutrho || $mode == 3 ]]; then
    echo 'density/energy cutoff ratio convergence'
    mode=3
    fol=conv3_ratio
    tag='ecutrho'
else
    echo 'Mode not recognized'
    exit 0
fi

mkdir -p $fol
cp $input  $fol

#ln -sf ../pseudo
cd $fol
ln -sf ../pseudo
shift
paras=($@)
length=${#paras[@]}
length=` expr $length - 1`
echo parameters to be tested: ${paras[@]}

for j in ` seq 0 $length `; do
    para=${paras[$j]}
    fol_j=test${j}_$para
    mkdir -p $fol_j
    cp $input $fol_j
    cd $fol_j

    if [[ $mode == 1 ]];then
        #echo ${kratio[0]} ${kratio[1]} ${kratio[2]} $para
        k1=` expr ${kratio[0]} \* $para `
        k2=` expr ${kratio[1]} \* $para `
        k3=` expr ${kratio[2]} \* $para `
        echo kgrid $k1 $k2 $k3
        kgrid="$k1 $k2 $k3 0 0 0"
        sed -i "/K_POINTS/{n;s/.*/$kgrid/}" $input
    else ## other modes
        echo $para
        sed -i "s/.*$tag.*/  $tag = $para/" $input
    fi
    cd ..

done

