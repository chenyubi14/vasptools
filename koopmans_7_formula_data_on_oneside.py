#[0]:
import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
sys.path.append(pwd)
from class0_functions1 import read_incar
from class1_read import read_file_values, rotate_read

# instruction:
# run koopmans6 first and then run this koopmans7 because koopmans6 saves FREYCORR data

#[1]:
# read the type of the folder from 'DEFECT', will know: interstitial, vacancy, perfect cells
mydictionary=read_incar(pwd, incar='DEFECT')
title_info = mydictionary['DEFECTTYPE']

#[2]:
#var='HFSCREEN';relax_fix_var='AEXX'
var='AEXX';relax_fix_var='HFSCREEN'

read_fil=read_file_values(pwd)
relax_fix_para=read_fil.incar([relax_fix_var, var])

#[3]:
read_fil=rotate_read(pwd)


## highest occupied energy: E_ho
x1,yy1=read_fil.rotate(var,'energyHOLO', 'koopmans',var,'0e_') # y1=epsilon_ho
yy1=yy1[:,0] # The negative of the highest occupied states # drop the nband in y1 

# yy3-yy2=the energy difference between positive defect E(N-1) and neutral defect E(N)
x2,yy2=read_fil.rotate(var,'equil_energy','koopmans', var,'0e_') #Neutral defect LOCPOT, E(N)
x3,yy3=read_fil.rotate(var,'equil_energy','koopmans', var,'1e_') #Postive defect LOCPOT, E(N-1)
assert np.all(x1==x2) and np.all(x1==x3)

# Then read Freysoldt correction from a data file
x4,yy4=read_fil.rotate(var,'freysoldt_corr','koopmans', '1e_'+var)

#  Koopmans' theorem: E(N-1)-E(N)+E_ho+Ecorr
y1=np.round(yy3-yy2+yy1+yy4,6) 

if var == 'HFSCREEN':
    numberofdata=5
    fity1=y1[:numberofdata]
    fitx1=x1[:numberofdata]
else:
    fity1=y1
    fitx1=x1
print('Fit this array to 0',fity1)
poly=np.polyfit(fity1,fitx1,1)# fitting function
po=np.poly1d(poly) # poly function
var_fit=np.round(po(0),3)
print('Fit %s=%s'%(var,var_fit))



# Ready for plot

#x1=x1.astype(str)
#y1=y1.astype(str) #Epos_m_Eneu_p_Eho_p_Ecorr
#print("\nx=np.array([%s])\nxlabel=\'%s\'\ny=np.array([%s])\nylabel=\'Koopmans\'\nlegend=\'$E(N-1)-E(N)+\epsilon_{ho}+E_{corr}$\'\ntitle=\'%s\'\ncomment=\'%s=%s_%s=%s_fit=%s\'\nplt.plot(x,np.zeros(len(x)), label=\'$E(N-1)-E(N)+\epsilon_{ho}+E_{corr}=0$\')\n" % (','.join(x1), var,','.join(y1), relax_fix_var, title_info, relax_fix_para[0],var,relax_fix_para[1], var_fit ) )
#print('\nCopy the above data to draw_temp.koopmans.py, and draw a diagram.')
#
#print('\n\nRun with \'python $SCRIPT/draw_temp.koopmans.py\'')

y1=np.round(yy3-yy2+yy4,6)
y2=np.round(-yy1,6)

x1=x1.astype(str)
y1=y1.astype(str) #Epos_m_Eneu_p_Eho_p_Ecorr
y2=y2.astype(str)
print('Calculate y=Epos-Eneu+E_ho. Expect y=0 satisfy koopmans condition\n')
print("\nx=np.array([%s])\nxlabel=\'%s\'\ny=np.array([%s])\nylabel=\'Koopmans\'\nylabelunit=\' (eV)\'\nlegend=\'$E(N-1)-E(N)$\'\nz=np.array([%s])\ntitle=\'%s\'\ncomment=\'%s=%s_%s=%s_fit%s=%s\'\nplt.plot(x,z,\'o\', label=\'$-\epsilon_{ho}$\')\n" % (','.join(x1), var,','.join(y1),','.join(y2), title_info, relax_fix_var, relax_fix_para[0],var,relax_fix_para[1],var, var_fit ) )
print('\nCopy the above data to draw_temp.koopmans.py, and draw a diagram.')

print('Run with \'python $SCRIPT/draw_temp.koopmans.py\'')