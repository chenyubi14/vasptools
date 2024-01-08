# In[1]:
import numpy as np
import os
import sys
import matplotlib.pyplot as plt

from pymatgen.core import Structure

# Goal: plot trace of force constants as a function of distance



# In[3]:
poscar='perfect/POSCAR'
if not os.path.exists(poscar):
    poscar='SPOSCAR'
structpos=Structure.from_file(poscar)
sites=structpos.sites
natom=len(structpos)

if len(sys.argv)<2:
    print('Error! Enter which atom (1,2,...) you want to plot force trace')
    sys.exit()
else:
    sel = int(sys.argv[1])

with open('FORCE_CONSTANTS', 'r') as f:
    lines = f.readlines()

x=structpos.distance_matrix[sel-1,:]
y=np.array([])




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
        print('atom%s %s,line=%s,force=%.3f' % (sel,i+1, ind+1,atomi_force[j]))
    trace=sum(atomi_force)
    y=np.append(y,trace)

arg=np.argsort(x)
#print(list(zip(x[arg],y[arg])))
speinfo=structpos.composition.get_el_amt_dict()
spe1num=speinfo[structpos[sel-1].specie.name] # element
for i in range(len(arg)):
    print('%.2s\t%.2s%.3s\t%s\t%.5f %.5f' % (arg[i]+1, structpos[arg[i]].specie.name, int(arg[i] % spe1num + 1) , structpos[arg[i]].frac_coords, x[arg[i]], y[arg[i]]) )
print(speinfo)
print('2nd species got % by ', spe1num)
# In[2]:  plot parameters
SMALL_SIZE = 12
MEDIUM_SIZE = 14
BIGGER_SIZE = 16

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
plt.figure()


title=''
xlabel='distance '
xlabelunit='$\AA$'
ylabel='force trace'
ylabelunit=''

figname='forcetrace_atom%s.pdf' % (sel)

# In[4]: plot

x=np.delete(x,sel-1)
y=np.delete(y,sel-1)
plt.plot(x, y, 'o', label='atom%s %s'%(sel,structpos[sel-1].specie.name) ) # legends as label=...
plt.legend()
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()
print(figname, ' is generated')
plt.savefig(figname, dpi=400)
