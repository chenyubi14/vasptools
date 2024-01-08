import numpy as np
from pymatgen.core import Structure

# Goal: if the interstitial position is in the extensive line from site 1 to site 2, generate site 3 with a certain distance from site 2
# POTCAR should not be copied from super_perfect folder !!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#poscar_file='POSCAR.draft1.iH.randomposition'
poscar_file='POSCAR.draft1.randomPosition'
stru=Structure.from_file(poscar_file) 
# in the extensive line from site 1 to site 2, generate site 3 with a certain distance from site 2
# pymatgen index = vesta index -1 = POSCAR line number - 10
# the first number below is (vesta_index - 1 + 1*True if atom inserted in front)
# note the vesta_index should be from the perfect POSCAR visualization, -1 is added specific to each case
distan=1.85 # (A) distance between site 3 and site 2
#ind1=84-1 # +1 if a Be atom is inserted before O atoms
#ind2=55-1 #
#ind_change=-1 # change the last atom
ind1=23-1 # +1 if a Be atom is inserted before O atoms
ind2=266-1 #
ind_change=-1 # change the last atom

site_change=stru[ind_change]
x1=stru[ind1].coords
x2=stru[ind2].coords
direction=np.array(x2-x1)
direction = (direction)/np.linalg.norm(direction) # make unit vector
x3=x2+direction*distan

#lattice.frac_coords gives abc, the direct coords 
#lattice.coords gives xyz, the Cartesian coords
site_change.coords=x3
atomtype=site_change.species_string
draftind=int(poscar_file[12])+1
stru.to(filename='POSCAR.draft%s.i%s.vasp'%(draftind,atomtype),fmt='POSCAR')