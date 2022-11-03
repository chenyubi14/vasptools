import sys
import os
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
sys.path.append(pwd)
from class3_smaller_folders import smaller_one_folder

# use submit=3 old with unit for vasp5, or submit=4 unit for vasp6
submit = 0 # 0 for don't submit, 1 for SUBMIT, 2 for SUBMITLARGE, 3 for SUBMITOLD, 4 for SUBMITUNIT

#dielectype=0 # # dielec_elec-part_eps: only electronic part # 
dielectype=1 # electronic and ionic part
#dielectype=2 # eps(omega) frequency dependence
#dielectype=3 # eps(omega) frequency dependence with a small bandgap
#dielectype=4 # eps(q) frequency and wave vector dependence

# Attention!
# (1) dielectype=0,1 are approximately q=0
# (2) dielectype=4 needs many unoccupied orbitals, because the calculation needs perturbation theory and thus a complete set of basis functions


smaller=smaller_one_folder(pwd)
smaller.dielec_const_eps(submit=submit, dielectype=dielectype)
