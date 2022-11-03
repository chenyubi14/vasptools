import numpy as np
import os 
import sys
from pymatgen.core import Structure

if len(sys.argv)<3:
    print('Error! Need to enter $1=unit poscar, $2=DIM')

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

with open('BORN.ALL','r') as f:
    lines=f.readlines()
    eps=np.array(lines[1].split()).astype(float)
    born=np.zeros([len(lines)-2,9])
    for i in range(2, len(lines)):
        born[i-2,:]=np.array(lines[i].split()).astype(float)
#print(eps,born)
#sys.exit()

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
    f.write('epsilon(:,1)=%.10f %.10f %.10f \n\tepsilon(:,2)=%.10f %.10f %.10f \n\tepsilon(:,3)=%.10f %.10f %.10f \n\t' % tuple(eps) )
    for i in range(len(born)):
        f.write('born(:,1,%s)=%.10f %.10f %.10f \n\tborn(:,2,%s)=%.10f %.10f %.10f \n\tborn(:,3,%s)=%.10f %.10f %.10f \n\t' % ( i+1,born[i,0],born[i,1],born[i,2], i+1,born[i,3],born[i,4],born[i,5], i+1,born[i,6],born[i,7],born[i,8] ) )
    f.write('scell(:)=%s \n&end\n' % (scell) )
    f.write('&parameters \n\tT_min=100 \n\tT_max=300 \n\tT_step=100 \n\tscalebroad=0.1 \n&end\n')
    f.write('&flags \n\tnonanalytic=.TRUE. \n\tnanowires=.FALSE. \n\tconvergence=.TRUE. \n&end\n')
