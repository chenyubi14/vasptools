#!/usr/bin/env python
import sys
import os
import copy
import argparse
import numpy as np
from pathlib import Path
from pymatgen.core import Structure


# Goal: a cleaner version of perturb: shift the neighbor atoms near the picked atom
print('Warning! Must choose an atom that is near the cell center. Otherwise, struc.index(neighbor) cannot locate the atom')

poscar = 'POSCAR'
newposcar = 'POSCAR.perturb.vasp'
struc=Structure.from_file(poscar)
sites=struc.sites


#In[0]:
parser = argparse.ArgumentParser(description='POSCAR perturb atoms')
parser.add_argument('sel', type=int, nargs='*',choices=range(0, len(sites)),
        help='Enter the selected atoms to perturb its neighbors')
parser.add_argument('--neighbor', metavar='n',type=float,dest='neighbor',default=2.5,
        help='Perturb within this range of the selected atom')
parser.add_argument('--percent', metavar='p',type=float,dest='percent',default=0.05, 
        help='percentage of perturbation')
args = parser.parse_args()
sel=args.sel # a list of atoms to perturb
neighbordis=args.neighbor # perturb atoms within this range near the "defect"
percent=args.percent
print('selected atoms:%s neighbor=%s, percent=%s' % (sel,neighbordis, percent) )


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


## neighbors of each atom to be perturbed, also perturb the defect site
for ind in sel:
    perturbsite = sites[ind]
    print('\nPerturn around atom %s%s %s' % (perturbsite.species_string, ind, perturbsite.frac_coords))
    # perturb neighbors and atom itself
    all_perturb=struc.get_neighbors(perturbsite,neighbordis) + [perturbsite]
    for neighbor in all_perturb:
        old_coord=copy.deepcopy(neighbor.frac_coords)
        neighborind = struc.index(neighbor)
        # Translate specific sites by some vector, keeping the sites within the unit cell.
        this_perturb=(np.random.rand(3)-0.5) * percent
        struc.translate_sites(neighborind, this_perturb) 
        print('Before: %s After: %s' % (old_coord, struc[neighborind].frac_coords) )

struc2poscar(struc,folder='./',fname=newposcar,selective_dynamics=False,atoms=[])
print('Generated %s' % newposcar )

