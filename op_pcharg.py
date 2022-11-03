import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
sys.path.append(pwd)
from class3_smaller_folders import smaller_one_folder
# Goal: submit a job to calculate the partial charges on a certain band
# This can be useful when out_wavefunc1.py does not generate pretty figures
# #	At this case, a density plot will be prettier
# PROCAR could not be generated when producing PARCHG

submit = 5 # 0 for don't submit, 1 for SUBMIT, 2 for large, 3 for old, 4 for many, 5 for short, 6 for single, 7 for test

smaller=smaller_one_folder(pwd)
#bands= list(np.arange(190,199+1))
bands= list(np.arange(568,580+1))
bands=np.array(bands).astype(str)
iband=' '.join(bands)
smaller.pcharg(iband=iband,submit=submit)
