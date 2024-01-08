
mkdir out
cd out
echo "ln -s ../../nscf/out/*.save"
cd ../
echo "ln -s ../wann/*.mat"
echo "ln -s ../wann/*.xyz"


echo "
&qe2pert
  prefix = 'si'
  outdir = './out'
  phdir = '../phonon/out'
  nk1 = 4
  nk2 = 4
  nk3 = 4
  dft_band_min = 1
  dft_band_max = 16
  num_wann = 8
  lwannier=.true.
  load_ephmat = .false.
  system_2d = .false.
/
" > in.qe2pert
