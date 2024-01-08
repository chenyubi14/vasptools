#!/usr/bin/env python
# coding: utf-8

# In[1]:

import os
import sys
import re

path = os.environ['PWD'] + '/'
sys.path.append(path)
sys.path.append(os.environ['SCRIPT'])

from class0_functions1 import find_files,read_incar
from class1_read import read_file_values
from class99_last_drawmulinfo import drawmulinfo
from class98_last_drawsingleinfo import drawsingleinfo

#ideal_u = 0.375

if len(sys.argv) > 1:
    print(sys.argv)
    print('Warning! Double check that whether you want to use op_var* ')
    sys.exit()

# In[2]:

var_test = 2 # 1: draw for a single variable, need to read many folders. 0: draw from a single task. 2:other
structure = None
middle = ''
from pathlib import Path
saveinfo=Path('SAVEINFO')
if saveinfo.is_file():
	materialcomment = read_incar('.',incar='SAVEINFO')['COMMENT'] # '$O$'
else:
	print('Should enter like COMMENT=Cd3As2 in the file SAVEINFO for graph title, need at least two keywords')
	saveinfo=open('SAVEINFO','w')
	saveinfo.write('COMMENT=\nlegend=\n')
	sys.exit()

#['grid/ENCUT/SIGMA/AEXX/bond_length/SCALING','equil_energy/lattice_para/bandgap/bond_length','']
if var_test == 1:
    # constrain to certain folders in formats header*middle*var
    # values to import
    #header='conv';mat=['grid', 'equil_energy', '']
    header='conv';mat=['grid', 'lattice_para', '']
    #header='conv';mat=['ENCUT', 'equil_energy', '']
    #header='conv';mat=['ENCUT', 'lattice_para', '']
    #header='conv';mat=['SIGMA', 'equil_energy', '']
    #header='conv';mat=['SIGMA', 'lattice_para', '']
elif var_test == 0:
    header = 'hse'
    mat=['DOSCAR', 'energy', 'PDOS']; legend = ['s', 'p', 'd']
    #mat=['DOSCAR', 'energy', 'DOS']
    son_folder = '' # if you are in the parent folder, go to son-folder
    path = path + son_folder
elif var_test == 2:
    #find files header and var
    #file_names=os.listdir('.')
    file_names=[name for name in os.listdir(".") if os.path.isdir(name)]
    ignore_files = ['__pycache__','setup_converge.py']
    # INCAR, KPOINTS, POTCAR, POSCAR, submit.job, SAVEINFO
    # These files should be ignored, but they don't affect the code of detecting _
    for ign in ignore_files:
        try:
            file_names.remove(ign)
        except:
            pass
    # removed filenames that should definitely not be considered
    not_found = True
    while not_found:
        fil_i = file_names.pop(0)
        fil_i = fil_i.split('_')
        if len(fil_i)>=2:
            header = fil_i[0]
            header = re.sub( r'[0-9]','', header) # (pattern, replacement, string)
            var = fil_i[-1]
            mat = [var, 'lattice_para','' ]
            # exclude the possibilities of pdf/png files
            try:
                assert var[-3:] != 'pdf' and var[-3:] != 'png'
                not_found=False
            except:
                pass

print('header=%s, var=%s' % (header, var))

#x_file_open = mat[0]
x_str = mat[0]
y_str = mat[1]
middle = mat[2]


# In[3]:
if var_test == 1 or var_test == 2:
    draw_x_data=drawmulinfo(x_str, y_str, header, middle=middle, lattice_structure=structure,materialcomment=materialcomment)
elif var_test == 0:
    draw_x_data=drawsingleinfo(x_str, y_str, header, middle=middle, lattice_structure=structure)
xx=draw_x_data.xx
data=draw_x_data.data


# In[4]:

draw_x_data.plot()


# In[ ]:




