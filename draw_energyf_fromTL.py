
# In[1]:

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import MultipleLocator

import sys
import os
sys.path.append(os.environ['SCRIPT'])
from class95_energyf_fromTL import energyf_fromTL, defectinfo

"""
Plot formation energy which come from other's calculations:
    assume E^f at E_F=0 is provided for each charge state
"""

# (1) defect information: type, formula, directories, charges
######################################################## check for changes
defecttype='$\mathrm{NV}$'
defect_charge = np.array([1,0,-1,-2])
energyf = np.array([7.435,5.905,5.312,6.900]) # formation energy at (E_F=3)
energyf = energyf-3*defect_charge # formation energy at VBM (E_F=0)
nv = defectinfo(defecttype,charge=defect_charge,energyf=energyf) # 

defecttype='$\mathrm{NH}$'
defect_charge = np.array([2,1,0,-1,-2])
energyf = np.array([11.956,9.620,8.497,8.245,9.771]) # formation energy at (E_F=3)
energyf = energyf-3*defect_charge # formation energy at VBM (E_F=0)
nh = defectinfo(defecttype,charge=defect_charge,energyf=energyf) # 

defecttype='$\mathrm{V}$'
defect_charge = np.array([2,1,0,-1,-2])
energyf = np.array([10.425,8.077,6.378,5.548,7.002]) # formation energy at (E_F=3)
energyf = energyf-3*defect_charge # formation energy at VBM (E_F=0)
v = defectinfo(defecttype,charge=defect_charge,energyf=energyf) # 

defecttype='$\mathrm{N}$'
defect_charge = np.array([1,0,-1])
energyf = np.array([3.645,4.349,5.688]) # formation energy at (E_F=3)
energyf = energyf-3*defect_charge # formation energy at VBM (E_F=0)
n = defectinfo(defecttype,charge=defect_charge,energyf=energyf) #


defecttype='$\mathrm{H}$'
defect_charge = np.array([1,0,-1])
energyf = np.array([6.643,6.107,6.880]) # formation energy at (E_F=3)
energyf = energyf-3*defect_charge # formation energy at VBM (E_F=0)
h = defectinfo(defecttype,charge=defect_charge,energyf=energyf) #


defecttype='$\mathrm{V_{2}H}$'
defect_charge = np.array([1,0,-1,-2])
energyf = np.array([9.495,7.630,6.934,7.199]) # formation energy at (E_F=3)
energyf = energyf-3*defect_charge # formation energy at VBM (E_F=0)
v2h = defectinfo(defecttype,charge=defect_charge,energyf=energyf) #


defecttype='$\mathrm{V_{2}}$'
defect_charge = np.array([2,1,0,-1,-2])
energyf = np.array([13.034,10.490,8.621,8.089,8.187]) # formation energy at (E_F=3)
energyf = energyf-3*defect_charge # formation energy at VBM (E_F=0)
v2 = defectinfo(defecttype,charge=defect_charge,energyf=energyf) #


defecttype='$\mathrm{NVH}$'
defect_charge = np.array([2,1,0,-1,-2])
energyf = np.array([9.781,6.908,4.579,4.129,5.455]) # formation energy at (E_F=3)
energyf = energyf-3*defect_charge # formation energy at VBM (E_F=0)
nvh = defectinfo(defecttype,charge=defect_charge,energyf=energyf) #
######################################################## 

# (2) all defects forming a list; run Freysoldt corrections
######################################################## check for changes
list_defect=[nvh,v2h,v2,h,nh,v,n,nv] #  # this file considers vacancies' formation energies
alpha = np.ones(len(list_defect)) # opacity in general
#alpha[0] = 0.5  # oapcity for each defect. Does not consistently change color of text and points so far
comment = 'mansoor.PBE' # this comment is used in the address of figure.
condition = 'diamond' # calculate delta miu under this condition

bandgap=5.4
######################################################## 


myread=energyf_fromTL(list_defect,bandgap, comment=comment, alpha=alpha, condition=condition)


######################################################### check for changes
plot_charge_labels_segment=False #False: don't plot charges. True: plot charges
lower_bound_plot=True
xlimits=[0, 5.4]
ylimits=[1, 10]
# where to put defect labels
# UpOrDown= 1 for Upward shift by one unit; 0 for no shift; -1 for Downward shift by one unit
if condition == 'diamond': # {defecty_onetype:[transition_level_ind,LeftOrRight,UpOrDown]}
    defectlabeltextposition={nv:[1,-0.3,-1,0],nh:[2,0,-1.5,0],n:[0,3.5,3,0],v:[3,0.2,0,0],h:[1,1.5,0.2,0],nvh:[2,2,-1.5,0],v2h:[1,1,-1.5,0],v2:[3,0.1,0.5,0]} 
else:
    print('Error! need to specify the condition to know where to put defect labels')
    sys.exit()
#########################################################


myread.myplot(plot_charge_labels_segment,lower_bound_plot=lower_bound_plot,defectlabeltextposition=defectlabeltextposition,xlimits=xlimits,ylimits=ylimits)
# calculate formation energy only when drawing
