#!/usr/bin/env python

import sys
import os
import numpy as np
from fractions import Fraction
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
sys.path.append(pwd)
from class0_functions1 import find_files
from class2_update_input import change_input_files
from class3_smaller_folders import smaller_folders
from pymatgen.core import Structure
from setup_converge import divnum, header
from setup_converge import func as kgrids_n


print('Do convergence on k-point grid')
fil = 'KPOINTS' # the file to edit
var = 'grid' # variable
MAX_DENOM = 100 # maximum denominator



# In[2]:

structpos=Structure.from_file('POSCAR')
rec_lat=structpos.lattice.reciprocal_lattice
abc=np.round(rec_lat.abc,5) # reciprocal lattice vectors
print('The BZ abc is %s' % abc )
# want the kpoints to follow this ratio abc
### This is the rigorous ratio1
#fractions = [Fraction(val).limit_denominator(MAX_DENOM)
#                     for val in abc.values()]
#factor = np.array([(f.numerator, f.denominator) for f in fractions])
factor = np.array([Fraction(val).limit_denominator(MAX_DENOM).as_integer_ratio()
                    for val in abc])
factor = np.lcm.reduce(factor[:,1])
ratio1 = np.array([round(v * factor) for v in abc])
### This is an approximate ratio2
ratio2 = abc / abc.min()

print('BZ abc is ', ratio2)
ratio2 = np.round(ratio2)
print('Suggested BZ abc ratios a=%s or b=%s ' % (ratio1,ratio2) )

if np.all(ratio2 == np.array([1.,1.,1.])):
    print('abc are the same, kgrid will be n*n*n')
    ratio = ratio2
else:
    print('What ratio do you want to use? a, b ,or enter three numbers')
    info=input().split()
    if info == ['a'] :
        print('Use ratio %s' % (ratio1))
        ratio=ratio1
    elif info == ['b']:
        ratio2 = np.round(ratio2)
        print('Use ratio %s' % (ratio2))
        ratio=ratio2
    elif len(info)==3:
        try: 
            ratio = np.array(info).astype(float)
        except:
            print('The inputs cannot be converted to integers')
            sys.exit()
    else:
        print('Input not recognized')
        sys.exit()



# In[3]:
print('ATTENTION! Will use POSCAR instead of CONTCAR')

# delect existing folders
find_files(pwd, header, var, remove=True) # yes, remove old directories


# create folders
# make directory names
new_folders = []
num_digits = int(np.log10(divnum)) + 1 

# edit files in each directory
for i in range(1, divnum+1): # index starts from 1. save index=0 for special cases
    new_folder = header + str(i).zfill(num_digits) +'_'+var
    new_folders.append(new_folder)
    #print(new_folder)
    os.mkdir(new_folder)
    os.system('cp INCAR POTCAR POSCAR ./'+new_folder)
    # edit KPOINTS
    kgrid = np.round(ratio / ratio[0] * kgrids_n[i-1] ).astype(int)
    kgrid = ' '.join(list(kgrid.astype(str)))
    print('kgrid%s: %s' % (i, kgrid) )
    with open(new_folder+'/KPOINTS','w') as f:
        f.write('K grid\n0\nG\n')
        f.write('%s \n' % kgrid)
    # edit INCAR
    changes = {'LWAVE':'F', 'LCHARG':'F'}
    change_files=change_input_files(new_folder)
    change_files.incar_change(changes, popkey=['LORBIT'])



