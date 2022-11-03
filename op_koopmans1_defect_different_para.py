import sys
import os
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
sys.path.append(pwd)
from class3_smaller_folders import smaller_folders

# submit many jobs with difference variable=var, and fix their ionic positions
# Assume DEFECT is updated
# Use 'out_koopmans0_*.py' to output data; Need to copy data to draw.temp.koopmans.py and draw.

submit = 5 # 0 for don't submit, 1 for SUBMIT, 2 for SUBMITLARGE, 3 for SUBMITOLD, 4 for SUBMITUNIT, 5 for SUBMITSHORT
smaller=smaller_folders(pwd)
#smaller.koopmans_rotate(submit=submit,var='HFSCREEN', varrange=[0,0.4], divnum=9)



# additional changes on charged states, does not affect the neutral states
additionalchanges = {'NUPDOWN':0.1} #'FERWE':'192*1.0 1*0.5 320*0.0','FERDO':'192*1.0 1*0.5 320*0.0', 'ISMEAR':-2, 'ICHARG':0

############################### change this!!!
q_change=-0.1
#q_change=0.0416
# varrange=[0.0,0.25], divnum=6
# varrange=[0.2,0.4], divnum=3
###############################
smaller.koopmans_rotate_one(submit=submit,var='AEXX', varrange=[0.0,0.40], divnum=3,q_change=q_change, additional_charged_changes=additionalchanges)
