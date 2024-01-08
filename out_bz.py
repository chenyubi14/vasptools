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

## or np.linalg.inv(latt_vec) can give (b1,b2,b3) as well
b1=2*np.pi*np.cross(a2,a3)/volume
b2=2*np.pi*np.cross(a3,a1)/volume
b3=2*np.pi*np.cross(a1,a2)/volume
reci_latt= structpos.lattice.reciprocal_lattice.matrix 
## reci_latt = np.linalg.inv(latt_vec).T * 2 * np.pi
#b1=reci_latt[0]
#b2=reci_latt[1]
#b3=reci_latt[2]

print('reciprocal lattice,  with 2pi \n%s\n%s\n%s' % (b1, b2, b3) )
#print('reciprocal lattice,  with 2pi \n%s' % (reci_latt) )
print('reciprocal lattice,  without 2pi \n%s\n%s\n%s' % (b1/2/np.pi, b2/2/np.pi, b3/2/np.pi) )

print('XYZ vectors in b1,b2,b3:\n', latt_vec.T)
