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
#for iband in fermi_xbands:
#	iband = iband
#	print('save for band=%s' % iband )
#	bandread = ebands3d(bs.efermi,kmesh, kpoints, energies, rlat, fermi_xbands=[iband])
#	bandread.to_bxsf(prefix='fourier_inter_%s_band%s' % (lpfac,iband), fermi_xbands_rename = [iband])




# generate the Fermi surface and calculate the dimensionality
fs = FermiSurface.from_band_structure(
  dense_bs, mu=0.0, wigner_seitz=True, calculate_dimensionality=True
)

# generate the Fermi surface and calculate the group velocity at the
# center of each triangular face
dense_kpoints = kpoints_from_bandstructure(dense_bs)
fs = FermiSurface.from_band_structure(
  dense_bs, mu=0.0, wigner_seitz=True, calculate_dimensionality=True,
  property_data=velocities, property_kpoints=dense_kpoints
)

# number of isosurfaces in the Fermi surface
print('number of isosurfaces in the Fermi surface', fs.n_surfaces)

# number of isosurfaces for each Spin channel
print('number of isosurfaces for each Spin channel',fs.n_surfaces_per_spin)

# the total area of the Fermi surface
print('total area of the Fermi surface',fs.area)

# the area of each isosurface
print('the area of each isosurface',fs.area_surfaces)

# loop over all isosurfaces and check their properties
# the isosurfaces are given as a list for each spin channel
for spin, isosurfaces in fs.isosurfaces.items():
    for isosurface in isosurfaces:
        # the dimensionality (does the surface cross periodic boundaries)
        isosurface.dimensionality

        # what is the orientation
        isosurface.orientation

        # does the surface have face properties
        isosurface.has_properties

        # calculate the norms of the properties
        isosurface.properties_norms

        # calculate scalar projection of properties on to [0 0 1] vector
        isosurface.scalar_projection((0, 0, 1))

        # uniformly sample the surface faces to a consistent density
        isosurface.sample_uniform(0.1)

# plot the Fermi surface
fs_plotter = FermiSurfacePlotter(fs)
plot = fs_plotter.get_plot()

# generate Fermi slice along the (0 0 1) plane going through the Γ-point.
fermi_slice = fs.get_fermi_slice((0, 0, 1))

# number of isolines in the slice
fermi_slice.n_lines

# do the lines have segment properties
fermi_slice.has_properties

# plot slice
slice_plotter = FermiSlicePlotter(fermi_slice)
plot = slice_plotter.get_plot()

save_plot(plot, "fermi-slice.png")  # saves the plot to a file
show_plot(plot)  # displays an interactive plot
