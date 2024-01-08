import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
#sys.path.append(pwd)
from class0_functions1 import find_files

#ref='bulk'
ref='defect'
f1='sync_freysoldt_ref-'+ref # folder of file 1
sys.path.append(pwd+f1)

from freysoldt_C_values import para_C

para_C=np.round(np.average(para_C, axis=1),4)
# para_C=para_C[:,0] # this picks a0.data as the C parameter
print('para_C with averaging three coods is %s' % (para_C))

header='freysoldt'
var='_correction_ref-'+ref

existed_folders=find_files(pwd, header=header, var=var, remove=False)
existed_folders = sorted(existed_folders)
print('The folders satisfied the pattern: \n',existed_folders)
for i in range(len(existed_folders)):
    # name of each sub-folder
    fol_name= pwd + existed_folders[i]
    os.chdir(fol_name)
    with open('sx.sh', 'r') as f:
        command = f.readline()
        ind=command.find('-C') # or -9
        command=command[:ind+3]
    with open('sx.sh', 'w') as f:
        command=command+'%s > sx2.fc\ncat sx2.fc'% (para_C[i])
        print(command)
        f.write(command)
        #os.system('sh sx.sh')
os.chdir(pwd)