#In[0]:
import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
#sys.path.append(pwd)
from class0_functions1 import find_files, read_incar
from class0_functions3 import write_DEFECT

# modify DEFECT with keyword and keyvalues
# Need to enter keyword and keyvalue
# Cannot update CENTER (the defect position). Should use inter_defct_position.py which uses classA_function_defect_*.py to do it.

#print(sys.argv, len(sys.argv))
if len(sys.argv)%2 !=1 :
    print("Error! Enter 'python *.py keyword keyvalues' ")
    sys.exit()
dictionary = read_incar(pwd, incar='DEFECT')
updateinfo=np.array(sys.argv[1:]).reshape(-1,2)
for i in range(len(updateinfo)):
    dictionary[updateinfo[i,0]] = updateinfo[i,1]
write_DEFECT(pwd, dictionary, incarname='DEFECT' )