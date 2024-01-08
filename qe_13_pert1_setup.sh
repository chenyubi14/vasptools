

echo "ln -sf ../qe2pert/epr.h5"
echo "perturbo.x -i in.pert > out.pert"

echo "
&perturbo
 prefix = 'gaas'
 calc_mode = 'bands'
 fklist = 'gaas_band.kpt'
/
" > in.pert

echo "
&perturbo
 prefix = 'gaas'
 calc_mode = 'phdisp'
 fqlist = 'gaas_phdisp.qpt'
/
" > in.pert

echo "
&perturbo
 prefix = 'gaas'
 calc_mode = 'ephmat'
 fklist = 'gaas_band.kpt'
 fqlist = 'gaas_band.qpt'

 band_min = 5
 band_max = 5
/
" > in.pert
