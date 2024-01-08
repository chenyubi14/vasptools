#!/usr/bin/env python

import sys
import os
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
sys.path.append(pwd)
from class3_smaller_folders import smaller_folders
from setup_converge import mat, divnum, func, header

# start with 4 input files; output folders 
# remember to write setup_convergence.py file.
# Will need the func, mat, divnum, header information from this file.
print(sys.argv,len(sys.argv))
if len(sys.argv) > 1:
    print('Warning! Double check that which op_var0* op_var1* you want to use')
    sys.exit()

# In[2]:

print('Do convergence on parameter ', mat, ' with ', divnum, ' values')
# filename and the variable to do convergence
fil = mat[0]
var = mat[1]
# header='hse' #'converge_'
# current folder is pwd
print('in folder: ', pwd)

submit = 0 # 0 for don't submit, 1 for SUBMIT, 2 for SUBMITLARGE, 3 for SUBMITOLD, 4 for SUBMITMANYK, 5 for SUBMITSHORT, 6 for SUBMITSINGLEK


# In[3]:

# run
change_dict={}
smaller=smaller_folders(pwd)
smaller.var_test(header, var, divnum, func, submit, change_dict=change_dict)

