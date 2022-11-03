# In[1]:
import numpy as np
from pymatgen.io.vasp.outputs import Wavecar
from pymatgen.io.vasp.outputs import Poscar
from pymatgen.core import Structure
import os
import sys
sys.path.append(os.environ['SCRIPT'])

# this file use WAVECAR to plot Charge density with colors
# Go to a graphdata folder, run 'python ${SCRIPT}/draw_wavefunc1.py kpoint band' 


print('Will generate the colored CHGCAR for the wavefunction of a kpoint, a band, and a spin.\nUse Vesta to visualize')
if len(sys.argv)>=4:
	ikpoint=int(sys.argv[1]) # integer value of kpoint
	iband=int(sys.argv[2])-1 # integer value of band (pymatgen will read EIGENVAL_band_index-1 )
	spinupdown=int(sys.argv[3]) # integer value spin up or down
else:
	print('Error! Enter kpoint (=0,1,..), band number (=EIGENVALUE band), spin up(=0) or down(=1)')
	sys.exit()

wv = Wavecar(filename='WAVECAR')
stru = Structure.from_file('CONTCAR')
pos = Poscar(stru)
parchg=wv.get_parchg(pos,kpoint=ikpoint,band=iband,spin=spinupdown,phase=True)

plotname='k%sband%sspin%s.CHGCAR.vasp' % (ikpoint,iband+1,spinupdown)
parchg.write_file(plotname)

print('Generate: %s'% plotname)