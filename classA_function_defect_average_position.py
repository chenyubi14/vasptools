#In[0]:
import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
#sys.path.append(pwd)
from class0_functions1 import find_files, read_incar
from class0_functions3 import write_INFO

# modify DEFECT position
# Need to enter defect positions individually

dictionary = read_incar(pwd, incar='SAVEINFO')
#print(sys.argv, len(sys.argv))
if len(sys.argv)!= 4:
    print('Error! Enter the position of defect to be CENTER individually')
    sys.exit()
vecpos=np.array(sys.argv[1:]).astype(float)
vecpos=np.round(vecpos,3)
dictionary['CENTER'] = '%s,%s,%s # the position of defect center' % tuple(vecpos) #(sys.argv[1],sys.argv[2],sys.argv[3])
write_INFO( dictionary, incarname='SAVEINFO' )
