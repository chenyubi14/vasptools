import numpy as np
import sys
import os
sys.path.append(os.environ['SCRIPT'])
from class1_read import read_file_values
path=os.environ['PWD']+'/'

read_fil = read_file_values(path)
phase = 'wurtzite' # 'rock-salt'
# read (print) lattice parameters
abc, abcnames = read_fil.lattice_para(structure=phase)
#for i,iname in zip(abc, abcnames):
#    print('%s\t%s=%s' % (phase,i, iname))

# special parameters like wurtzite u
if phase == 'wurtzite':
    u,_ = read_fil.get_u()
    print('wurtzite u parameter is %s' % (u))
#elif phasenum == 'rock-salt':
#    pass
#else:
#    print('The phase is not recognized')


