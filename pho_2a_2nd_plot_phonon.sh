echo 'create FORCE_SETS by phonopy...'
phonopy -f dis*/vasprun.xml
# a file called FORCE_SETS will be created
#rm -f dis*/WAVECAR dis*/CHGCAR

# Phonon band structure
echo 'plotting phonon dispersions'
phonopy -p -s band.conf
mv band.pdf band.nonac.pdf

echo 'Got phonon band structure with command:'
echo -e '\tphonopy -p -s band.conf\n'
echo -e 'use non analytical correction: run phonopy-vasp-born in the unit cell with LEPSILON turned on. It will read vasprun.xml and generate the content for the BORN file'
echo -e 'ln -s ../BORN'
echo -e '\tphonopy -p -s --nac band.conf; mv band.pdf band.nac.pdf \n'


# Phonon DOS
echo  'edit mesh.conf: increase MP until DOS does not change much'
echo 'get phonon DOS'
echo -e '\tphonopy -p -s mesh.conf\n'
# -p means plot
# -s means save


# Thermal properties
echo 'get thermal properties'
echo -e '\tphonopy -p -s -t mesh.conf\n'
# -t calculate thermal properties
# set TMAX, TMIN, and TSTEP in mesh.conf to change the ranges of the figure


phonopy-bandplot  --gnuplot > band.plot.dat
echo 'band.nonac.pdf and band.plot.dat generated'
