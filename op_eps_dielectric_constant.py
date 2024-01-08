#!/usr/bin/env python
import sys
import os
import argparse
import numpy as np
sys.path.append(os.environ['SCRIPT'])
from class3_smaller_folders import smaller_one_folder

pwd = os.environ['PWD'] + '/'
sys.path.append(pwd)

# use submit=3 old with unit for vasp5, or submit=4 unit for vasp6
submit = 0 # 0 for don't submit, 1 for SUBMIT, 2 for SUBMITLARGE, 3 for SUBMITOLD, 4 for SUBMITUNIT

parser = argparse.ArgumentParser(description='Type of dielectric constant calculations')
parser.add_argument('dielectype', type=int, choices=np.arange(0,4),
        help='0: only electronic, 1: electronic and ionic, 2: frequency dependence, 3 frequency dependence with a small band gap, 4: frequency and wave vector dependency')

args = parser.parse_args()
dielectype = args.dielectype

#dielectype=0 # # dielec_elec-part_eps: only electronic part # 
#dielectype=1 # electronic and ionic part
#dielectype=2 # eps(omega) frequency dependence
#dielectype=3 # eps(omega) frequency dependence with a small bandgap
#dielectype=4 # eps(q) frequency and wave vector dependence

# Attention!
# (1) dielectype=0,1 are approximately q=0
# (2) dielectype=4 needs many unoccupied orbitals, because the calculation needs perturbation theory and thus a complete set of basis functions


smaller=smaller_one_folder(pwd)
smaller.dielec_const_eps(submit=submit, dielectype=dielectype)
