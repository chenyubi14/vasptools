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
    spinupdown=int(sys.argv[2]) # integer value spin up or down
    #iband=int(sys.argv[3])-1 # integer value of band (pymatgen will read EIGENVAL_band_index-1 )
    bands = sys.argv[3:]
    bands = np.array(bands).astype(int) - 1
else:
	print('Error! Enter kpoint (=0,1,..), spin up=0/down=1, band numbers (=EIGENVALUE band)  1 2 3 4')
	sys.exit()

wv = Wavecar(filename='WAVECAR')
stru = Structure.from_file('CONTCAR')
pos = Poscar(stru)
for iband in bands:
    parchg=wv.get_parchg(pos,kpoint=ikpoint,band=iband,spin=spinupdown,phase=True)
    plotname='k%sband%sspin%s.CHGCAR.vasp' % (ikpoint,iband+1,spinupdown)
    parchg.write_file(plotname)
    print('Generate: %s'% plotname)
