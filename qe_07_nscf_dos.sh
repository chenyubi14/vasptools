#!/usr/bin/env bash



fil=in.qe
temp=in.temp

sed -i "s/.*calculation.*/  calculation = 'nscf'/" $fil
#sed -i "s/.*occupations =.*/  occupations = 'tetrahedra'/" $fil
echo '(1) update KPOINTS in in.qe by kmesh.pl'
echo -e '\t kmesh.pl 3 3 3 >> in.qe \n\t vim in.qe (old kpoint delete)'
echo '(2) might need tetrahedra for DOS (auto kmesh in this case)'

fil2=in.dos
rm -f $fil2
echo -e '&dos\n  DeltaE = 0.1\n/' >> $fil2
echo "run $fil  with pw.x binary"
echo "run $fil2 with dos.x binary"
