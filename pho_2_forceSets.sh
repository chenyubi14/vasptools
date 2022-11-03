echo 'create FORCE_SETS by phonopy...'
phonopy -f dis*/vasprun.xml
# a file called FORCE_SETS will be created
#rm -f dis*/WAVECAR dis*/CHGCAR

# Phonon band structure
echo 'edit band.conf: the BAND path and labels'
#echo '
# ATOM_NAME = Cr Al C
# DIM = 4 4 1
# PRIMITIVE_AXIS = AUTO
# BAND = 0.00 0.00 0.00   0.50 0.00 0.00   0.3333 0.3333 0.00   0.00 0.00 0.00   0.00 0.00 0.50   0.50 0.00 0.50   0.3333 0.3333 0.50   0.00 0.00 0.50
# BAND_POINTS = 101
# BAND_CONNECTION = .TRUE.
#' > band.conf
###
echo 'plotting phonon dispersions'
phonopy -p -s band.conf
mv band.pdf band.nonac.pdf

echo 'get phonon band structure'
echo -e '\tphonopy -p -s band.conf\n'
echo -e 'use non analytical correction: run phonopy-vasp-born in the unit cell with LEPSILON turned on. It will read vasprun.xml and generate the content for the BORN file'
echo -e '\tphonopy -p -s --nac band.conf\n'


# Phonon DOS
echo  'edit mesh.conf: increase MP until DOS does not change much'
# echo 'ATOM_NAME = 
# DIM = 
# MP = 20 20 20' > mesh.conf
###
echo 'get phonon DOS'
echo -e '\tphonopy -p -s mesh.conf\n'
# -p means plot
# -s means save


# Thermal properties
echo 'get thermal properties'
echo -e '\tphonopy -p -s -t mesh.conf\n'
# -t calculate thermal properties
# set TMAX, TMIN, and TSTEP in mesh.conf to change the ranges of the figure


