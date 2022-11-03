from pymatgen.electronic_structure.boltztrap2 import *
from pymatgen.electronic_structure.plotter import BSPlotter,DosPlotter
from monty.serialization import loadfn
import matplotlib.pyplot as plt
import os
import sys

material_type1=[ '1', '2']
material_type2=[ 'cd3as2', 'bamnsb2' ]
argv=sys.argv

if len(argv)<2:
	print('Error! Should enter what materials. %s for %s' % (material_type1, material_type2) )
	sys.exit()
elif argv[1] in material_type1:
	material = material_type2[ material_type1.index(argv[1]) ]
	print('material is %s' % material )
else:
	print('material type not recognized. Should enter ')


###################
data = VasprunBSLoader.from_file('../vasprun.xml')
print('data read from vasprun.xml. Read interpolated data...')
bztInterp = BztInterpolator(data,load_bztInterp=True,fname='bztInterp.json.gz')
###################




print('Interpolation coefficients read successfully. Try the band structure interpolation')
# define the path as a list
if material == 'bamnsb2':
	kpaths = [['G','Y','M','G','X','M']]
	kp_lbl = {'G': np.array([0.0, 0.0, 0.0 ]), 'Y':np.array([0.0,0.5,0.0]),'M': np.array([0.5, 0.5, 0.0 ]), 'G':np.array([0.0, 0.0, 0.0 ]) ,'X':np.array([0.5,0.0,0.0]) , 'M': np.array([0.5, 0.5, 0.0]) }
	ylim=[-2,1.0]
elif material == 'cd3as2':
	kpaths = [['Z','G','X']]
	kp_lbl = {'Z': np.array([0.5, 0.5, -0.5 ]), 'G':np.array([0.0,0.0,0.0]),'X': np.array([0., 0., 0.5 ])}
	ylim=[-1,0.5]
sbs = bztInterp.get_band_structure(kpaths,kp_lbl)

bsplot=BSPlotter(sbs) # .show()
bsplot.get_plot(zero_to_efermi=True)
ax = plt.gca()
ax.set_ylim(ylim)
figname='btp2_band.pdf'
plt.savefig(figname)
print('Figure saved as %s' % (figname) )


#print('DOS plotting...')
#fig,ax=plt.subplots(1,1)
#tot_dos = bztInterp.get_dos()
## set progress=True to show a progress bar
#tot_proj_dos = bztInterp.get_dos(partial_dos=False,progress=False)
#pltdos = DosPlotter(sigma=0.01)
##pltdos.add_dos_dict(tot_proj_dos.get_element_dos())
##pltdos.add_dos_dict(tot_proj_dos.get_densities())
##plt.savefig('btp2_dos.pdf')

