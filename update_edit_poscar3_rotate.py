#!/usr/bin/env python
import sys
import os
from pymatgen.core import Structure
import numpy as np
from pathlib import Path
import argparse

folder=os.environ['PWD']+'/'


#print('Will read POSCAR. Also recommend VASPKIT to generate an orthogonal supercell!!')
#print('Edit rot_matrix and super_array in this python file. By default, just rewrite')

##############################################################################
## wurtzite unit cell needs to be changed to orthogonal
### BeO 96-atom cell
# orthomatrix=[[1, 1, 0], [1, -1, 0], [0, 0, 1]];superarray=[3, 2, 2]
### BeO 288-atom cell
# orthomatrix=[[1, 1, 0], [1, -1, 0], [0, 0, 1]];superarray=[4, 3, 3]
### GaSb
#orthomatrix=[[1, 0, 0], [0, 1, 0], [0, 0, 1]];superarray=[2, 2, 2]
### Cd3As2
#orthomatrix=[[1, 0, 0], [0, 1, 0], [0, 0, 1]];superarray=[2, 2, 2]

parser = argparse.ArgumentParser(description='POSCAR perturb atoms')
parser.add_argument('--super', type=int, nargs=3,default=[1,1,1],
        help='three integers to construct supercell ')
parser.add_argument('--matrix',type=float, nargs=9, default=[1,0,0,0,1,0,0,0,1],
        help='9 elements of the rotation matrix')
args = parser.parse_args()
superarray = args.super
orthomatrix = args.matrix
orthomatrix = np.array(orthomatrix).reshape((3,3))

##############################################################################


def struc2poscar(struc, folder, fname='POSCAR', selective_dynamics=False, atoms=[]):
    folder = Path(folder)
    speinfo=struc.composition.get_el_amt_dict()
    spe1num=speinfo[struc[0].specie.name] 
    if selective_dynamics:
        selective_dynamics = [[True]*3]*(struc.num_sites)
        for atom in atoms:
            atom = int(atom)
            selective_dynamics[atom] = [False]*3
            print('fix atom%s %s%s %s' % (atom, struc[atom].specie.name, int(atom % spe1num ), struc[atom].frac_coords))
        print('Python indices: index + 1 to match with Vesta')
        struc.to(filename=str(folder/fname), selective_dynamics=selective_dynamics, fmt='poscar')
    else:
        struc.to(filename=str(folder/fname), fmt='poscar')


#In[1]:
struct = Structure.from_file('POSCAR')
ortho = struct * orthomatrix
superstruct = ortho * superarray

struc2poscar(superstruct, folder, fname='POSCAR', selective_dynamics=False, atoms=[])


