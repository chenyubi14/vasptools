import numpy as np
import sys
import os
sys.path.append(os.environ['SCRIPT'])

from class1_read import read_file_values

pwd = os.environ['PWD'] + '/'
read_fil = read_file_values(pwd)
band_gap = read_fil.bandgap()
print('The bandgap is %s eV'%(band_gap))