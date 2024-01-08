#!/usr/bin/env bash

echo 'first copy scf/out/ to phonon/'
echo -e '\t cp -r ../scf/out/ phonon/'

fil0=in.qe

fil1=in.1.phonon
rm -f $fil1 ## clean the old version
echo -e '&inputph' >> $fil1
sed -n '/prefix/p' $fil0 >> $fil1

echo -e "  outdir = './out'
  tr2_ph = 1.0d-13
  ldisp = .true.
!  epsil = .true.
  lqdir = .true.
  nq1 = 3
  nq2 = 3
  nq3 = 3
  fildyn  = 'harm_dyn'
  fildvscf = 'dvscf'
/" >> $fil1

echo '(1) edit nq1, nq2, nq3
(2) edit tr2_ph = 0.01*conv_thr
(3) kpoints in in.3.matdyn'

echo "run $fil1 with ph.x binary"

fil2=in.2.q2r
echo -e "&input
  fildyn = 'harm_dyn'
  zasr = 'simple'
  flfrc = 'harm_fc'
/" >> $fil2
echo "run $fil2 with q2r.x binary"

fil3=in.3.matdyn
echo -e "&input
  asr = 'simple'
  fildyn = 'harm_dyn'
  flfrc = 'harm_fc'
  flfrq = 'harm_freq'
  q_in_band_form = .true.
  q_in_cryst_coord = .true.
/
2
0.000 0.000 0.000 30 ! Gamma
0.000 0.000 0.500 30 ! Z
" >> $fil3
echo "run $fil3 with matdyn.x binary"

#fil4=in.4.plotband
#echo -e "
#" >> $fil4
#echo "run $fil4 with plotband.x binary"

