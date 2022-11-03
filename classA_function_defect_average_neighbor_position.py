import sys
import os
import numpy as np
import pymatgen as pmg
sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import read_incar
from class0_functions3 import write_DEFECT

# Update DEFECT CENTER by averaging over neighrbors
# this function is embedded into 'inter_defect_update_CENTER.sh'. Don't need to call from outside.
path=os.environ['PWD']+'/'

# Get neighbors' indices
neighborstr='neighbor'
posfil='CONTCAR'
defectinfo_raw=read_incar(path, incar='DEFECT')['NLINE']
assert defectinfo_raw[:len(neighborstr)] == neighborstr, 'Not vacancy defect'
#print('average over neighbors to get defect position')
defectinfo=np.array(defectinfo_raw.split(' ')[1:]).astype(int)
#print(defectinfo)

# Get defect position by averaging
from pymatgen.core import Structure
stru=Structure.from_file(path+posfil)
neighbors=[]
for nline in defectinfo:
    # nline-10 is the index of sites. nline is the line number shown in vim
    neighbor_i = stru[nline-10] 
    #print(neighbor_i.frac_coords)
    neighbors.append(list(neighbor_i.frac_coords)) # average over the fractional coordinates
#print(neighbors)
neighbors=np.array(neighbors)
defectposition=np.round(np.average(neighbors,axis=0),6)
defectposition = list( defectposition.astype(str) )
defectposition = ','.join(defectposition)
print('defect position by averaging neighbors: CENTER=%s'%(defectposition))

# Write the information to DEFECT
dictionary = read_incar(path, incar='DEFECT')
dictionary['CENTER']=defectposition+' # the position of defect center'
write_DEFECT(path, dictionary, incarname='DEFECT' )
#with open('DEFECT', 'w') as f:
#    f.write('CENTER=%s # the position of defect center\n' % (defectposition)) 
#    f.write('NLINE=%s\n'% (defectinfo_raw))
