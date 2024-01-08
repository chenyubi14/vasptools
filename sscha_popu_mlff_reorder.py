#!/usr/bin/env python
import sys
import os
import argparse
import numpy as np
from pathlib import Path
from pymatgen.core import Structure
#sys.path.append(os.environ['SCRIPT'])


parser = argparse.ArgumentParser(description='force and energy distances')
parser.add_argument('ref', type=str, 
        help='reference folder')
parser.add_argument('run', type=str, 
        help='run folder')
args = parser.parse_args()
ref=args.ref # a list of atoms to perturb
run=args.run # perturb atoms within this range near the "defect"
fil = 'vasprun.xml'



if os.path.isdir(ref) and os.path.isdir(run):
    print('Reorder force in atom order sscha_run=%s and mlff_run=%s ' % (ref, run) )
else:
    print('one of or both input folders do not exist!')
    sys.exit()

def get_diff_atom_force(sites1, sites2, for1, ):
    map_order = []
    for i in range(len(sites2)):
        ind = sites1.index(sites2[i])
        map_order.append(ind)
    #print('map from mlff to reference',map_order)
    ## reorder sites1, for1
    new_order_sites = np.array(sites1)[map_order]
    assert np.all(new_order_sites == np.array(sites2) ), 'Reorder does not work properly!'
    new_order_force = np.array(for1)[map_order]
    return  new_order_force


files = os.listdir(ref)
data = []
force_ele1 = np.array([])
force_ele2 = np.array([])
for fol_i in files:
    ref_fol = str(Path(ref)/ fol_i )
    if os.path.isdir(ref_fol):
        print(fol_i)
        run_fol = str(Path(run)/ fol_i )
        new_stress_file = ref_fol + '/stress_file'
        new_force_file = ref_fol + '/force_file'
        ## stress difference
        #ref_stress = np.loadtxt( ref_fol + '/stress_file', dtype=float )
        run_stress = np.loadtxt( run_fol + '/stress_file', dtype=float )
        ## force absolute difference
        ref_poscar = Structure.from_file( ref_fol + '/POSCAR')
        run_poscar = Structure.from_file( run_fol + '/POSCAR')
        #num_atom = ref_poscar.num_sites
        #ref_force = np.loadtxt( ref_fol + '/force_file', dtype=float )
        run_force = np.loadtxt( run_fol + '/force_file', dtype=float )
        ## force difference per atom 
        new_force1 = get_diff_atom_force( run_poscar.sites, ref_poscar.sites, run_force,)
        np.savetxt(new_force_file, new_force1)
        np.savetxt(new_stress_file, run_stress)


