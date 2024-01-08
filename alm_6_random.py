# In[1]:
import numpy as np
import sys
import os
import argparse
from pathlib import Path
from pymatgen.core import Structure
from pymatgen.io.vasp.outputs import Xdatcar

# Goal: add Random displacements to 40 selected jobs, run static VASP 


parser = argparse.ArgumentParser(description='Setup for random jobs')
parser.add_argument('--number',metavar='n', type=int, nargs=1,default=40,
        help='Enter the number of random jobs')
parser.add_argument('--percent',metavar='p', type=int, nargs=1,default=0.1,
        help='perturbation in Angstrom')
args = parser.parse_args()
num_sel = args.number
percent = args.percent


structs = Xdatcar('XDATCAR').structures
num_strucs = len(structs)
interval = int( num_strucs / num_sel )
sel_strucs = structs[:: interval]
num_sel = len(sel_strucs)
print('Total=%s, interval=%s, selected=%s' % (num_strucs, interval, num_sel ))


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

os.mkdir('random/')
num_digits = 1+int(np.log10(num_sel))
for i in range(num_sel):
    struct_i = structs[i]
    struct_i.perturb(percent)
    fol_i = 'random/dis_%s' % ( str(i).zfill(num_digits) )
    os.mkdir(fol_i)
    os.system('cp INCAR KPOINTS POTCAR '+fol_i)
    struc2poscar(struct_i, fol_i, fname='POSCAR')


