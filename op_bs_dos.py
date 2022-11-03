# coding: utf-8

# In[1]:

import numpy as np
import sys
import os
sys.path.append(os.environ['SCRIPT'])
from class3_smaller_folders import smaller_one_folder

# produce PROCAR in dos_non-self. It is because pcharg does not produce PROCAR even if IORBIT is set to 11.

# In[2]:

# Non-self consistent calculations to draw DOS or band structure
submit = 0 # 0 for don't submit, 1 for SUBMIT, 2 for SUBMITLARGE, 3 for SUBMITOLD, 4 for SUBMITUNIT
bs_dos = 0 # 0 for band structure, 1 for DOS and PDOS
pwd = os.environ['PWD'] + '/' # current working directory

if bs_dos == 0:
	print('check whether different K sampling is consistent, otherwise use more electronic steps by setting NELMIN')

calc_ops=smaller_one_folder(pwd) # calculation operations
calc_ops.calc_bs_dos(bs_dos, submit)

