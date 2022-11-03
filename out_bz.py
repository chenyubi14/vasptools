import numpy as np
import sys 
import os

from pymatgen.core import Structure
pwd = os.environ['PWD'] + '/' 


# calculate the brillouin zone from lattice vectors in CONTCAR

poscar='CONTCAR'
structpos=Structure.from_file(poscar) # read structure 
latt_vec=structpos.lattice.matrix
a1=latt_vec[0]
a2=latt_vec[1]
a3=latt_vec[2]
volume=structpos.lattice.volume

b1=2*np.pi*np.cross(a2,a3)/volume
b2=2*np.pi*np.cross(a3,a1)/volume
b3=2*np.pi*np.cross(a1,a2)/volume

print('reciprocal lattice,  with 2pi \n%s\n%s\n%s' % (b1, b2, b3) )
print('reciprocal lattice,  without 2pi \n%s\n%s\n%s' % (b1/2/np.pi, b2/2/np.pi, b3/2/np.pi) )
