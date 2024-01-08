#!/usr/bin/env python
from pymatgen.io.vasp.outputs import Vasprun
from ifermi.surface import FermiSurface
from ifermi.interpolate import FourierInterpolator
from ifermi.plot import FermiSlicePlotter, FermiSurfacePlotter, save_plot, show_plot
from ifermi.kpoints import kpoints_from_bandstructure
import numpy as np
import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
from fermi_5_write_bxsf import ebands3d


# load VASP calculation outputs
vr = Vasprun("../vasprun.xml")
bs = vr.get_band_structure()

# interpolate the energies onto a dense k-point mesh
filename='input.btp2'
if os.path.exists(filename):
        dicti = np.loadtxt(filename,comments=['!','#'], dtype=str,delimiter='=')
        dicti = dict(zip(dicti[:,0],dicti[:,1]))
        lpfac =int( dicti['lpfac'] )
        fermi_xbands = dicti['fermi_xbands']
        fermi_xbands = [ int(band) for band in fermi_xbands.split() ]
elif len(sys.argv) < 3:
        print('Error! Should enter an integer: lpfac')
        sys.exit()
else:
        lpfac=int(sys.argv[1])
print('lpfac=%s ' % (lpfac) )
interpolator = FourierInterpolator(bs) # interpolation factor = 5 by default
dense_bs,kmesh, kpoints, energies,rlat, velocities = interpolator.interpolate_bands(return_velocities=True,nworkers=1,interpolation_factor=lpfac)

spinkey=list(energies.keys())[0]
energies = energies[spinkey].T # energies[spinkey] is (band_num, kpoint_num), should flip the shape
rlat = rlat._matrix # get the matrix of reciprocal lattice
rlat = rlat / 2 /np.pi # remove 2pi in reciprocal lattice definition
for iband in fermi_xbands:
	iband = iband
	print('save for band=%s' % iband )
	bandread = ebands3d(bs.efermi,kmesh, kpoints, energies, rlat, fermi_xbands=[iband])
	bandread.to_bxsf(prefix='fourier_inter_%s_band%s' % (lpfac,iband), fermi_xbands_rename = [iband])


