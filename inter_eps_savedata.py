# store the result in EPS_DIELEC_CONST 

import sys 
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
path = os.environ['PWD'] + '/' 
from class1_read import read_file_values

# This file saves a one-dimensional electronic dielectric constant 
# To save a two dimentional dielectric constants, goto draw_out_var3D.py and choose z_str=eps, you will get the data at savedDATA/

# Read all dielectric constant
print('Should start with the folder at dielectric constant')
read_fil = read_file_values(path)
eps=read_fil.outcar_dielec_const(tensor=True) # should modify the function of reading full dielectric constant!
print('The dielectric constant is %s' % (eps))
with open(os.environ['WORK']+'/ALLINFO','w') as f:
    f.write('EPS=tensor %s,0,0,0' % (','.join(eps.astype(str))))


# Read electronic dielectric constant
rotate_f=rotate_read(path)
# x_str, y_str, header,var
#header = 'hsev610_'
#header = 'hsev544_'
header = 'v610aexx0.34_'
header = 'v610aexx0.32_'
var='HFSCREEN'
x_str = 'HFSCREEN'
xx,yy=rotate_f.rotate(x_str, 'eps', header,var) # x_str, y_str, header,var
savedata(path,xx,yy,var,'EPS',header=header) # (folder2nd,x,y,xname='x',yname='y',header='')
print(xx,yy)
