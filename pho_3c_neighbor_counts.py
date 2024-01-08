# In[1]:
import numpy as np
import os
import sys
import matplotlib.pyplot as plt

from pymatgen.core import Structure

# Goal: count neighbors along distances

if len(sys.argv)<2:
    print('Error! Enter which atom (1,2,...) you want to print force trace')
    sys.exit()
else:
    sel = int(sys.argv[1])

# In[3]:
poscar='perfect/POSCAR'
if not os.path.exists(poscar):
    poscar='SPOSCAR'
structpos=Structure.from_file(poscar)
natom=len(structpos) # number of atoms
sites=structpos.sites # lattice atom sites
rec_lat=structpos.lattice.reciprocal_lattice # get reciprocal lattice

max_rec_lat = np.max(rec_lat.abc) # maximum of the vector (a*, b*, c*)
maximum_radius = np.pi / ( max_rec_lat )
print('Maximum inscribed sphere radius is %s' % maximum_radius )


assert sel>0 and sel<=natom, 'Wrong atom index!'

with open('FORCE_CONSTANTS', 'r') as f:
    lines = f.readlines()

x=structpos.distance_matrix[sel-1,:].round(5)
y=np.array([])
unique_dist = np.unique(x)
unique_dist = list(np.sort(unique_dist))



last=1+4*natom*(sel-1)
print('start reading from line of',last)

for i in range(natom): 
    # atom 1 with another atom
    atomi_force = np.zeros(3)
    for j in range(3):
        ind=last+4*i+1+j # 3*i is the three lines of force constants
        # +2 means the first line, and also the second line
        # +1*i means the line to consider "atom 1 1"
        # j means the j-th line 
        line = lines[ind].split()
        atomi_force[j] = round(float(line[j]),5)
        #print('atom%s %s,line=%s,force=%.3f' % (sel,i+1, ind+1,atomi_force[j]))
    trace=sum(atomi_force)
    y=np.append(y,trace)

arg=np.argsort(x)
#print(list(zip(x[arg],y[arg])))
speinfo=structpos.composition.get_el_amt_dict()
spe1num=speinfo[structpos[sel-1].specie.name] # element
print('Index\tSpecies\tAtom_site\t\t\t\tDistance Force_trace Neighbor Inside?')
for i in range(len(arg)):
    print(
            '%.2s\t%.2s%.3s\t%s\t%8.5f %8.5f\t%s %s' % (
            arg[i]+1, 
            structpos[arg[i]].specie.name, 
            int(arg[i] % spe1num + 1) , 
            structpos[arg[i]].frac_coords, 
            x[arg[i]], 
            y[arg[i]],
            unique_dist.index(x[arg[i]]) , 
            True if x[arg[i]] < maximum_radius else False)
            )
print(speinfo)
print('2nd species got % by ', spe1num)


