#!/usr/bin/env python
import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import read_incar
#from class2_update_input import change_input_files

print('Check INCAR parameters, may need to edit cell-size-dependent variables')
incar_dict = read_incar('./')
all_keys = incar_dict.keys()

warn_keys=['NCORE','MAGMOM','NBANDS', 'NELECT']
for ikey in warn_keys:
    if ikey in all_keys:
        print('Warning! %s=%s' % (ikey,incar_dict[ikey]))
print('SOC needs to double NBANDS, because spin up/down will be separate')
