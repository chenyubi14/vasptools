from pymatgen.electronic_structure.boltztrap2 import VasprunBSLoader, BztInterpolator, BztPlotter
from pymatgen.electronic_structure.plotter import BSPlotter,DosPlotter
from monty.serialization import loadfn
import matplotlib.pyplot as plt
import os
import sys
import numpy as np

material_type1=[ '1', '2', '3']
material_type2=[ 'cd3as2', 'bamnsb2', 'PbTe' ]
argv=sys.argv

############################################################################
filename='input.btp2'
if os.path.exists(filename):
	dicti = np.loadtxt(filename,comments=['!','#'], dtype=str,delimiter='=')
	dicti = dict(zip(dicti[:,0],dicti[:,1]))
	lpfac =int( dicti['lpfac'] )
	energy_range=float( dicti['energy_range'] )
	material = dicti['material']
elif len(argv)<2:
	print('Error! Should enter what materials. %s for %s' % (material_type1, material_type2) )
	sys.exit()
elif argv[1] in material_type1:
	material = material_type2[ material_type1.index(argv[1]) ]
else:
	print('material type not recognized. Should enter ')
	sys.exit()
print('material is %s' % material )
############################################################################



############################################################################
data = VasprunBSLoader.from_file('../vasprun.xml')
print('data read from vasprun.xml. Read interpolated data...')
bztInterp = BztInterpolator(data,energy_range=energy_range,load_bztInterp=True,fname='bztInterp.json.gz')
############################################################################




############################################################################
print('Interpolation coefficients read successfully. Try the band structure interpolation')
# define the path as a list
if material == 'bamnsb2':
    kpaths = [['G','Y','M','G','X','M']]
    kp_lbl = {'G': np.array([0.0, 0.0, 0.0 ]), 'Y':np.array([0.0,0.5,0.0]),'M': np.array([0.5, 0.5, 0.0 ]), 'G':np.array([0.0, 0.0, 0.0 ]) ,'X':np.array([0.5,0.0,0.0]) , 'M': np.array([0.5, 0.5, 0.0]) }
    elim=[-1, 1]
elif material == 'cd3as2':
    kpaths = [['Z','G','X']]
    kp_lbl = {'Z': np.array([0.5, 0.5, -0.5 ]), 'G':np.array([0.0,0.0,0.0]),'X': np.array([0.,0.,0.5]), 'P': np.array([0.25,0.25,0.25]), 'N':np.array([0.,0.5,0.]), 'G':np.array([0.0,0.0,0.0]) }
    elim=[-1, 0.5]
elif material == 'GaSb':
    kpaths = [['G','X','K','G','L']]
    kp_lbl = {'G':np.array([0.0,0.0,0.0]),'X': np.array([0.5, 0.0, 0.5 ]),'K': np.array([0.375,0.375,0.75]), 'L': np.array([0.5,0.5,0.5]), 'U':np.array([0.625,0.25,0.625]), 'W':np.array([0.5,0.25,0.75]) }
    elim=[-5, 9]
elif material == 'PbTe':
    kpaths = [['G','X','W','K','G','L','U']]
    kp_lbl = {'G':np.array([0.0,0.0,0.0]),'X': np.array([0.5, 0.0, 0.5 ]),'K': np.array([0.375,0.375,0.75]), 'L': np.array([0.5,0.5,0.5]), 'U':np.array([0.625,0.25,0.625]), 'W':np.array([0.5,0.25,0.75]) }
    elim=[-5, 9]
elif material == 'hex':
    kpaths = [['G','M','K','G','A']]
    kp_lbl = {'G':np.array([0.0,0.0,0.0]),'M': np.array([0.5, 0.0, 0.0 ]),'K': np.array([0.333,0.333,0.00]), 'A': np.array([0.0,0.0,0.5]),}
    elim=[-5, 9]
elif material == 'graphene':
    kpaths = [['G','M','K','G']]
    kp_lbl = {'G':np.array([0.0,0.0,0.0]),'M': np.array([0.5, 0.0, 0.0 ]),'K': np.array([0.333,0.333,0.00]), 'A': np.array([0.0,0.0,0.5]),}
    elim=[-2, 2]
sbs = bztInterp.get_band_structure(kpaths,kp_lbl)

bsplot=BSPlotter(sbs)
bsplot.get_plot(zero_to_efermi=True)
ax = plt.gca()
ax.axhline(y=0,linestyle='--',color='black')
ax.set_ylim(elim)
figname='btp2_band.pdf'
plt.savefig(figname)
print('Figure saved as %s' % (figname) )
############################################################################


print('DOS plotting...')

## projected DOS
#fig,ax=plt.subplots(1,1)
# set progress=True to show a progress bar
#tot_dos = bztInterp.get_dos(partial_dos=False,progress=False) 
#tot_proj_dos = bztInterp.get_dos(partial_dos=True,progress=False)
#pltdos = DosPlotter(sigma=0.01)
# Need LORBIT=True + partial_dos=True to have element, spd projections
#pltdos.add_dos_dict(tot_proj_dos.get_element_dos())
#plt.savefig('btp2_partial_dos.pdf')

bztPlotter = BztPlotter(bzt_interp=bztInterp)
smooth_T = 0
pltdos=bztPlotter.plot_dos(T=smooth_T, npoints=40000)
#pltdos=bztPlotter.plot_dos( npoints=40000)
plt = pltdos.get_plot()
plt.xlim(elim)
#plt.ylim([0,1500])
#plt.ylim([0,500])
plt.tight_layout()
figname= 'btp2_total_dos.T%s.pdf' % (smooth_T) 
plt.savefig(figname)
print('%s saved' % figname )
