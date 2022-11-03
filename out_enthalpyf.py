import numpy as np
import sys
import os
sys.path.append(os.environ['SCRIPT'])

from class1_read import read_file_values
from class0_functions2 import hcf, get_printformula

pwd = os.environ['PWD'] + '/'

compound_folder = pwd # read native material formation enthalpy by default
if len(sys.argv)>1:
	compound_folder = sys.argv[1] # read formation enthalpy for a different compound like Li2O instead


read_fil = read_file_values(pwd)
enthalpyf,formula,speci_name,dict_elementformula = read_fil.read_formation_enthalpy(compound_folder, return_unit_formula=True)