# coding: utf-8

# In[1]:

import numpy as np
import sys
import os
sys.path.append(os.environ['SCRIPT'])
from class3_smaller_folders import smaller_one_folder

# produce PROCAR in dos_non-self. It is because pcharg does not produce PROCAR even if IORBIT is set to 11.

# In[2]:
if len(sys.argv)<2:
	print('Error! Enter 0(band structure non-weighted points), 1(density of states), 2 + line-mode-KPOINTS(band structure line-mode)')
	sys.exit()
mode=int(sys.argv[1])
assert mode==0 or mode==1 or mode==2, 'Error! Mode not recognized. Enter 0(bs hse), 1(dos), 2(bs line-mode)'
if mode == 2 and len(sys.argv)<3:
	print('Enter one more argument for the /path/to/line-mode/KPOINTS that you want to use')
	sys.exit()
elif mode == 2:
	kpath = sys.argv[2]
	#print('Will use KPOINTS ')

# Non-self consistent calculations to draw DOS or band structure
submit = 0 # 0 for don't submit, 1 for SUBMIT, 2 for SUBMITLARGE, 3 for SUBMITOLD, 4 for SUBMITUNIT
bs_dos = mode # 0 for band structure in general, 1 for DOS and PDOS, 2 for line-mode band structure
pwd = os.environ['PWD'] + '/' # current working directory

if bs_dos != 1:
	print('check whether different K sampling is consistent, otherwise use more electronic steps by setting NELMIN')
else:
    print('DOS calculation need denser K grid!')

calc_ops=smaller_one_folder(pwd) # calculation operations
if mode == 2:
	calc_ops.calc_bs_dos(bs_dos, submit, kpath=kpath)
else:
	calc_ops.calc_bs_dos(bs_dos, submit)

