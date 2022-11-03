import numpy as np
import sys
import os
sys.path.append(os.environ['SCRIPT'])

from class1_read import read_file_values
from class0_functions2 import hcf, get_printformula

pwd = os.environ['PWD'] + '/'

# output delta miu of native elements and impurity elements
# should input the condition like O-rich / Be-rich / Zn-rich

impurity_atomnames = []
if len(sys.argv)<2:
	print('Error! Need to enter condition like O-rich/Be-rich. Can compute impurity delta miu by including more arguments\nExample arguments: O-rich Li F H')
	sys.exit()
elif len(sys.argv) == 2:
	condition = sys.argv[1] #'O-rich'
elif len(sys.argv) > 2:
	condition = sys.argv[1]
	impurity_atomnames = sys.argv[2:] # read formation enthalpy for a different compound like Li2O instead

read_fil = read_file_values(pwd)
dict_delta_miu=read_fil.read_delta_miu4energyf(condition,impurity_atomnames=impurity_atomnames)
