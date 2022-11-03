#!/usr/bin/env python
# coding: utf-8

# In[1]:

import os
import sys

path = os.environ['PWD'] + '/'
sys.path.append(path)
sys.path.append(os.environ['SCRIPT'])

from class0_functions1 import find_files,read_incar
from class1_read import read_file_values
from class99_last_drawmulinfo import drawmulinfo
from class98_last_drawsingleinfo import drawsingleinfo

#ideal_u = 0.375


# In[2]:

var_test = 1 # 1: draw for a single variable, need to read many folders. 0: draw from a single task. 2:other
structure = None
middle = ''
materialcomment = read_incar('.',incar='DEFECT')['COMMENT'] # '$O$'

if var_test == 1:
    # constrain to certain folders in formats header*middle*var
    #header = 'size'
    header = 'conv'
    #header = 'convex0.0_'
    #header = 'koopmans'
    #header = 'largedN_'
    #header = 'janakdNispin2_' #'ispin1'
    # values to import
    #mat=['NELECT', 'equil_energy', '_'] # x_str(also var), y_str, middle
    #mat=['NELECT', 'hoenergy', ''] # x_str(also var), y_str, middle
    #mat=['AEXX', 'lattice_para', '']; structure = 'wurtzite'
    #mat=['AEXX', 'equil_energy','']
    #mat=['AEXX', 'wurtzite_u', '']
    mat=['ENCUT', 'equil_energy', '']
    #mat=['ENCUT', 'bond_length', '']
    #mat=['bond_length', 'equil_energy', '']
    #mat=['SCALING', 'equil_energy', '']
    #mat=['SCALING', 'bond_length', '']
elif var_test == 0:
    header = 'hse'
    mat=['DOSCAR', 'energy', 'PDOS']; legend = ['s', 'p', 'd']
    #mat=['DOSCAR', 'energy', 'DOS']
    son_folder = '' # if you are in the parent folder, go to son-folder
    path = path + son_folder


#x_file_open = mat[0]
x_str = mat[0]
y_str = mat[1]
middle = mat[2]


# In[3]:
if var_test == 1:
    draw_x_data=drawmulinfo(x_str, y_str, header, middle=middle, lattice_structure=structure,materialcomment=materialcomment)
elif var_test == 0:
    draw_x_data=drawsingleinfo(x_str, y_str, header, middle=middle, lattice_structure=structure)
xx=draw_x_data.xx
data=draw_x_data.data


# In[4]:

draw_x_data.plot()


# In[ ]:




