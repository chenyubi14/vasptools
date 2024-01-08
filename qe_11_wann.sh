#!/usr/bin/env bash


if [[ -z $1 || -z $2 ]]; then
    echo 'Error! Enter header a,b,c and prefix'
    exit 0
fi

header=$1

pp=pseudo
f1=${header}1_wann
f2=${header}2_qe2pert
f3=${header}3_pert-band
f4=${header}4_pert-ph
f5=${header}5_pert-ephmat

mkdir -p $f1 $f2 $f3 $f4 $f5

prefix=$2

echo "
&inputpp
  outdir = './out/'
  prefix = '$prefix'
  seedname = '$prefix'
  spin_component = 'none'
  write_mmn = .true.
  write_amn = .true.
  write_unk = .false.
/
" > $f1/in.pw2wan
echo In $f1
#echo "ln -sf ../nscf/out/prefix.save"
echo "(1) mkdir out; ln -sf ../../nscf/out/prefix.save"
echo "(2) kmesh.pl 3 3 3 wan >> $prefix.win"
echo "(3) wannier90.x -pp $prefix"
echo "(4) use pw2wannier90.x to run in.pw2wan"
echo "(5) wannier90.x $prefix"
