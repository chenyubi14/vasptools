#!/usr/bin/env bash



fil=in.qe

sed -i "s/.*calculation.*/  calculation = 'bands'/" $fil
sed -i "s/.*K_POINTS.*/K_POINTS {crystal_b}/" $fil
echo 'Do not forget to set nbnd value to include conduction electrons'
echo -e '\treplace the kgrid with kpath from high-symmetry kpoints, example:'
echo -e 'K_POINTS {crystal_b}\n3\n  0.000 0.000 0.000 20\n  0.000 0.000 0.500 20\n  0.500 0.500 0.500 20'

fil2=in.band
rm -f $fil2 ## clean the old version
#echo -e '&bands\n' >> $fil2
echo -e '&bands' >> $fil2
sed -n '/prefix/p' $fil >> $fil2
echo -e "  outdir = './out/'\n  filband = 'bands.dat'\n/" >> $fil2

echo "(0) cp -r ../scf/out/ ."
echo "(1) run $fil with qe.x binary"
echo "(2) run $fil2 with bands.x binary"
