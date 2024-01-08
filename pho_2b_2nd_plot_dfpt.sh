echo 'create FORCE_CONSTANTS by phonopy. Must have FORCE_CONSTANTS=READ in band.conf!'
phonopy --fc dfpt/vasprun.xml
# a file called FORCE_CONSTANTS will be created

# Phonon band structure
echo 'plotting phonon dispersions'
phonopy -p -s band.conf
##This should be 'phonopy -p -s --dim="2 2 2" -c POSCAR-unitcell band.conf'
mv band.pdf band.nonac.pdf

echo 'Got phonon band structure with command:'
echo -e '\tphonopy -p -s band.conf\n'
echo -e 'use non analytical correction: run phonopy-vasp-born in the unit cell with LEPSILON turned on. It will read vasprun.xml and generate the content for the BORN file'
echo -e '\tphonopy -p -s --nac band.conf\n'


phonopy-bandplot  --gnuplot > band.plot.dat
echo 'band.nonac.pdf and band.plot.dat generated'
