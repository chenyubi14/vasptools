import numpy as np
import os 
import sys
from pymatgen.core import Structure

if len(sys.argv)<3:
    print('Error! Need to enter $1=unit poscar, $2=DIM')
    sys.exit()

unitpos=sys.argv[1]
struct=Structure.from_file(unitpos)
latt=struct.lattice.matrix
spec=struct.species


elements=[]
types=[]
for i in range(len(spec)):
    name= '\"' + spec[i].name + '\"'
    if name not in elements:
        elements.append(name)
    types.append(str(len(elements)))
print('elements=%s, types=%s' % (elements, types) )


scell=' '.join( sys.argv[2:6]) # dimension

# write CONTROL file
with open('CONTROL', 'w') as f:
    f.write('&allocations \n\tnelements=%s \n\tnatoms=%s \n\tngrid(:)=10 10 10 \n&end\n' % (len(elements), len(struct) ))
    f.write('&crystal \n\tlfactor=0.10000 \n\t' )
    for i in range(3):
        vec=latt[i,:]
        f.write('lattvec(:,%s)=%.10f %.10f %.10f \n\t' % (i+1, vec[0], vec[1], vec[2]))
    f.write('elements=%s \n\ttypes=%s \n\t' % ( ' '.join(elements), ' '.join(types)) )
    for i in range(len(struct)):
        pos=struct[i].frac_coords
        f.write('positions(:,%s)=%.10f %.10f %.10f \n\t' % (i+1, pos[0], pos[1], pos[2]))
    f.write('scell(:)=%s \n&end\n' % (scell) )
    f.write('&parameters \n\tT_min=100 \n\tT_max=300 \n\tT_step=100 \n\tscalebroad=0.1 \n&end\n')
    f.write('&flags \n\tnonanalytic=.TRUE. \n\tnanowires=.FALSE. \n\tconvergence=.TRUE. \n&end\n')
