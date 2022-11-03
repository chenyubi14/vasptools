#!/usr/bin/env python
# coding: utf-8

# In[1]:

import os
import sys

path = os.environ['PWD'] + '/'
sys.path.append(path)
sys.path.append(os.environ['SCRIPT'])

from class0_functions1 import find_files
from class1_read import read_file_values
from class99_last_drawmulinfo import drawmulinfo3D

#In[1]:
# output a data depending on two dimensions
# or draw a contour plot or a 3D plot
# to make two plots together, read the data saved to write a temporary .py code
# e.g. draw bandgap and koopmans together used this method


#Maybe useful
#ideal_u = 0.375
structure = None

# In[2]:

draw_plot= True # True: Draw the plot. False: save the data only



# values to import. Format is 
#[x_file_open, x_str, y_str, z_str, middle]
#mat=['INCAR', 'AEXX', 'lattice_para']; structure = 'wurtzite'
header = 'v620aexx';mat=['INCAR', 'HFSCREEN', 'AEXX', 'bandgap', '']
#mat=['INCAR', 'HFSCREEN', 'AEXX', 'eps', '']
#header = 'koopmansAEXX';mat=['INCAR', 'HFSCREEN', 'AEXX', 'koopmans', 'charge0e_']  # Koopmans condition needs this


# In[3]:

x_file_open = mat[0]
x_str = mat[1] # x should be the last variable in var(y)_i_var(x)
y_str = mat[2] # y should be the previous variable
# this is for the classification of data
z_str = mat[3] # contour plot of z
middle = mat[4]

draw_x_data=drawmulinfo3D(x_str, y_str, z_str, header, middle, lattice_structure=structure)
#xx=draw_x_data.xx
#data=draw_x_data.data


# In[4]:
if draw_plot:
    draw_x_data.plot()


# In[ ]:




