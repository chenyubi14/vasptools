#[1]:
import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
#print('In Python',pwd)
from class1_read import read_file_values, rotate_read

#[2]:
var='AEXX'
#var='HFSCREEN'

#[3]:
#read_fil=read_file_values(pwd)
#read_fil.locpot2freysoldt_correction()
read_fil=rotate_read(pwd)

## Koopmans' theorem right-side: E_ho
#x1,y1=read_fil.rotate(var,'energyHOLO', 'koopmans','0e_'+var)
#y1=-y1[:,0] # The negative of the highest occupied states # drop the nband in y1 
#
## Koopmans' theorem left-side part: E(N-1)-E(N), the energy difference between positive defect E(N-1) and neutral defect E(N)
## calculate the energy difference between E(N-1)-E(N), 
#x2,yy2=read_fil.rotate(var,'equil_energy','koopmans', '0e_'+var) #Neutral LOCPOT, E(N)
#x3,yy3=read_fil.rotate(var,'equil_energy','koopmans', '1e_'+var) #Postive LOCPOT, E(N-1)
#y2=np.round(yy3-yy2,6) # E(N-1)-E(N)
#assert np.all(x1==x2) and np.all(x1==x3)
#x1=x1.astype(str)
#y1=y1.astype(str)
#y2=y2.astype(str)
#print('\nx=np.array([%s])\nE_ho=np.array([%s])\nEpos_Eneu=np.array([%s])\n' % (','.join(x1),','.join(y1),','.join(y2)))
# Then read Freysoldt correction from a data file
x4,y4=read_fil.rotate(var,'run_freysoldt','koopmans', '1e_'+var)
print('freysoldt_corr is set to 0.0 for print conveniences. The output is not yet ready.')

##print('\nCopy the above data to a python file in a local computer, and draw a diagram.')


## If another form of Koopmans theorem
#a1,b1=read_fil.rotate('AEXX','energyHOLO', 'koopmans','1e_AEXX')
#b1=-b1[:,0] # The negative of the highest occupied states # drop the nband in y1 
#assert np.all(x1==a1) 
#a1=a1.astype(str)
#b1=b1.astype(str)
#x1=x1.astype(str)
#y1=y1.astype(str)
#print('\nx=np.array([%s])\nE_ho=np.array([%s])\nE_lo=np.array([%s])\n' % (','.join(x1),','.join(y1),','.join(b1)))

