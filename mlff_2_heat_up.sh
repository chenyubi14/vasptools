#!/usr/bin/env bash

inter=inter.txt
ls -d *_TEBEG > $inter

fols=($(sort -n $inter) )
echo ${fols[@]} ${#fols[@]}
length=${#fols[@]}
length=`expr $length - 2 `

for i in ` seq 0 ${length}`;do
    j=`expr $i + 1 `
    echo $i $j ${fols[$i]}  ${fols[$j]} 
    pre=${fols[$i]}
    current=${fols[$j]} 
    ln -sf ../${pre}/CONTCAR ${current}/POSCAR
    ln -sf ../${pre}/ML_ABN ${current}/ML_AB
done

rm $inter

