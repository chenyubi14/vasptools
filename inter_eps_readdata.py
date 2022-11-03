import sys 
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
path = os.environ['PWD'] + '/' 
from class0_functions1 import read_incar
from class0_functions3 import get_2D_electronic_eps

# dielectric constant eps(AEXX, HFSCREEN) is calculated for a matrix of data. However, for a given folder with AEXX/HFSCREEN, eps are not necessarily calculated 
# This function is to interpolate eps for the current folder 'path'. The 2D data is stored in FIRSTFOL like /work2/07884/tg871834/stampede2/berylliumO/ELECTRONIC_EPS.py 
# To update ELECTRONIC_EPS.py (two-dimentional dielectric constants), goto draw_out_var3D.py and choose z_str=eps, you will get the data at savedDATA/
dictionary=read_incar(path, incar='INCAR')
dict_find={}
dict_find['AEXX']=dictionary['AEXX']
dict_find['HFSCREEN']=dictionary['HFSCREEN']
# dict_find = {'AEXX':0.4, 'HFSCREEN':0.2}
this_eps=get_2D_electronic_eps(dict_find) # interpolate eps with arguments in dict_find
print('eps %.3f' % this_eps)
