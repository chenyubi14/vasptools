import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
sys.path.append(pwd)
from class3_smaller_folders import smaller_one_folder
# the partial charge a certain band
# PROCAR could not be generated when producing PARCHG

submit = 0 # 0 for don't submit, 1 for SUBMIT, 2 for large, 3 for old, 4 for many, 5 for short, 6 for single, 7 for test

smaller=smaller_one_folder(pwd)
smaller.hyperfine(submit=submit)
