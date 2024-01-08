#In[0]:
import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
#sys.path.append(pwd)
from class0_functions1 import find_files, read_incar
from class0_functions3 import write_INFO


#In[1]:
#ref='bulk'
#var_koopmans_fol='HFSCREEN'
var_koopmans_fol='AEXX'



#In[2]:
ref='defect'
header='freysoldt'
var='_correction_ref-'+ref
existed_folders=find_files(pwd, header=header, var=var, remove=False)
existed_folders = sorted(existed_folders)
print('The folders satisfied the pattern: \n',existed_folders)

#In[3]:
Ecorr=[]
for i in range(len(existed_folders)):
    # name of each sub-folder
    fol_name= pwd + existed_folders[i]
    os.chdir(fol_name)
    with open('sx2.fc', 'r') as f:
        lines=f.readlines()
        line=lines[-1]
        Ecorr.append(float(line.split('(eV):')[1].split('(incl.')[0]))
        folder_koopmans = 'koopmans'+existed_folders[i][9]+'_charge1e_' + var_koopmans_fol
        dictionary = read_incar(pwd+folder_koopmans, incar='SAVEINFO')
        dictionary['FREYCORR'] = Ecorr[-1]
        write_INFO(dictionary,pwd+folder_koopmans, incarname='SAVEINFO' )
Ecorr=np.array(Ecorr).astype(str)
print('\nEcorr=np.array([%s])\n' % (','.join(Ecorr)))
