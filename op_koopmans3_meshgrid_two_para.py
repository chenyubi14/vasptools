import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
sys.path.append(pwd)
from class3_smaller_folders import smaller_folders

# submit a meshgrid of jobs with two variables, and fix their ionic positions
# Assume DEFECT is updated
# first use rotate.sh to run freysoldt correction, which uses inter_defect_update_freysoldtcorr.sh for each folder
# then use draw_out_var3D.py to analyze and plot data
# last copy data in savedDATA/ to draw_temp.bandgap.koopmans.py

def func_varrange(varrange_i, div_i):
    return np.round(np.linspace(varrange_i[0],varrange_i[1], div_i),6)

submit = 5 # 0 for don't submit, 1 for SUBMIT, 2 for SUBMITLARGE, 3 for SUBMITOLD, 4 for SUBMITUNIT, 5 for SUBMITSHORT
# The first variable
var1='AEXX'
div1=7
varrange1=[0.32,0.5] # ([0.32, 0.35, 0.38, 0.41, 0.44, 0.47, 0.5 ])
# The second variable
var2='HFSCREEN'
div2=5 #9
varrange2=[0,0.2]

varrange=[func_varrange(varrange1,div1),func_varrange(varrange2,div2)]
smaller=smaller_folders(pwd)
smaller.koopmans_rotate_multi(submit=submit,var=[var1,var2], varrange=varrange)
