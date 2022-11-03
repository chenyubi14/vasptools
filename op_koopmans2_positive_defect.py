import sys
import os
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
#sys.path.append(pwd)
from class2_update_input import change_input_files
from class3_smaller_folders import smaller_folders
from class0_functions1 import pythonsubmit,read_incar
# from class1_read import read_file_values


# submit one job with difference variable=var, and fix their ionic positions
# Assume DEFECT is updated
# Use 'out_koopmans0_*.py' to output data for a perfect cell; Need to copy data to draw.temp.koopmans.py and draw.
# Use 'out_koopmans1,...,7_*.py' to output data for a defect cell.

submit = 0 # 0 for don't submit, 1 for SUBMIT, 2 for SUBMITLARGE, 3 for SUBMITOLD, 4 for SUBMITUNIT, 5 for SUBMITSHORT
# often use submit=5
smaller=smaller_folders(pwd)
#smaller.koopmans_rotate(submit=submit,var='HFSCREEN', varrange=[0,0.4], divnum=9)
# q_change changes the charges of the cell: q_change < 0 means adding electrons; q_change>0 means removing electrons
#q_change=1
q_change=-18
#q_change=-0.1
#q_change=-0.6
#q_change=0.0416

# additional changes on charged states, does not affect the neutral states
additionalchanges = {'ISPIN':1} #'FERWE':'192*1.0 1*0.5 320*0.0','FERDO':'192*1.0 1*0.5 320*0.0', 'ISMEAR':-2, 'ICHARG':0


aexx_current = read_incar(pwd)['AEXX']
############################### change this!!!
# varrange=[0.0,0.25], divnum=6
# varrange=[0.2,0.4], divnum=3
# varrange=[aexx_current,1], divnum=1
###############################
smaller.koopmans_rotate_one(submit=submit,var='AEXX', varrange=[0, 0.4], divnum=3,q_change=q_change, additional_charged_changes=additionalchanges)
