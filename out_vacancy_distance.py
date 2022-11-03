import numpy as np
import sys
import os
sys.path.append(os.environ['SCRIPT'])

from class0_functions1 import read_incar
from class1_read import read_file_values
from class0_functions2 import hcf, get_printformula

from pymatgen.core import Structure
pwd = os.environ['PWD'] + '/'

# calculate how far the neighbors of vacancy deviate from their original positions

poscar='CONTCAR'
structpos=Structure.from_file(poscar) # read structure from POSCAR
sites=structpos.sites

defectinfo=read_incar(pwd, incar='DEFECT')
if 'NLINENEIGHBOR' in defectinfo.keys():
	nline=defectinfo['NLINENEIGHBOR'].split()[1:] # if the defect has an atom, NLINENEIGHBOR stores the NLINE of its neighbors
	# need to start with neighbor
else:
	nline=defectinfo['NLINE'].split()[1:] # defect atom is vacancy, NLINE stores the neighbor information
defectposition=np.array(defectinfo['POSITIONNORELAX'].split()).astype(float) #np.array([0.50000, 0.41652, 0.25018 ])
print('Position of origin=%s' % defectposition )
distan=[]
for neighbor_i in nline:
	neighbor_i = int(neighbor_i)-10
	neighbor_pos = sites[neighbor_i]
	print('(vesta) neighbor=%s frac_coords=%s' % (neighbor_i+1, neighbor_pos.frac_coords))
	distan.append(neighbor_pos.distance_and_image_from_frac_coords(defectposition)[0])
distan = np.round(distan, 5)
distan = ', '.join(list(distan.astype(str)))
print('\nimport numpy as np')
print('distances=np.array([%s])# in order of neighbors ' % (distan))
print('f=lambda x,ref:(x-ref)/ref')

beo_references = [1.61677,1.61677,1.6214,1.62877]
beo_references_name = ['Be_18/O_64','Be_14/O_60','Be_8/O_54','Be_32/O_78']
print('#BeO references')
#for ref in beo_references:
#	print('ref=%s;f(distances,ref)'% ref)
for i in range(len(beo_references)):
	ref = beo_references[i]
	beo_label = beo_references_name[i]
	print('ref=%s;f(distances,ref) # %s'% (ref,beo_label))

zno_references = [1.96480,1.96480,1.96382,1.97366]
zno_references_name = ['Zn38/O_85','Zn42/O_89','Zn32/O_79','Zn8/O_56']
print('#ZnO references')
for i in range(len(zno_references)):
	ref = zno_references[i]
	zno_label = zno_references_name[i]
	print('ref=%s;f(distances,ref) # %s'% (ref,zno_label))
