#!/usr/bin/env python
import sys
import os
import argparse
import numpy as np
from pathlib import Path
from pymatgen.core import Structure
sys.path.append(os.environ['SCRIPT'])
from class1_read import read_file_values
#from pymatgen.io.vasp.outputs import BSVasprun
import matplotlib.pyplot as plt

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
    print('Calculate energy and force distances with reference=%s and mlffrun=%s ' % (ref, run) )
else:
    print('one of or both input folders do not exist!')
    sys.exit()

def get_diff_atom_force(sites1, sites2, for1, for2):
    map_order = []
    for i in range(len(sites2)):
        ind = sites1.index(sites2[i])
        map_order.append(ind)
    #print('map from mlff to reference',map_order)
    ## reorder sites1, for1
    new_order_sites = np.array(sites1)[map_order]
    assert np.all(new_order_sites == np.array(sites2) ), 'Reorder does not work properly!'
    new_order_force = np.array(for1)[map_order]
    rmse = np.linalg.norm(new_order_force - for2) / np.sqrt(len(sites1))
    maxdiff = np.max(np.abs(new_order_force - for2))
    return rmse, maxdiff, new_order_force, for2


files = os.listdir(ref)
data_str = ['E(reference)\tE(MLFF)\tE(diff)[eV]\t\tF_mag(reference)\tF_mag(MLFF)\tF_mag(diff)\tF_ele(diff_rmse)\tF_ele(diff_max)[eV/A]']
data = []
force_ele1 = np.array([])
force_ele2 = np.array([])
stress_ele1 = np.array([])
stress_ele2 = np.array([])
for fol_i in files:
    ref_fol = str(Path(ref)/ fol_i )
    if os.path.isdir(ref_fol):
        print(fol_i)
        run_fol = str(Path(run)/ fol_i )
        ref_class_i = read_file_values( ref_fol )
        run_class_i = read_file_values( run_fol )
        ## energy difference
        ref_energy = ref_class_i.oszicar()
        run_energy = run_class_i.oszicar()
        diff_energy = ref_energy - run_energy
        ## stress difference
        ref_stress = np.loadtxt( ref_fol + '/stress_file', dtype=float )
        run_stress = np.loadtxt( run_fol + '/stress_file', dtype=float )
        stress_ele1 = np.append(stress_ele1, ref_stress)
        stress_ele2 = np.append(stress_ele2, run_stress)
        ## force absolute difference
        ref_poscar = Structure.from_file( ref_fol + '/POSCAR')
        run_poscar = Structure.from_file( run_fol + '/POSCAR')
        num_atom = ref_poscar.num_sites
        ref_force = np.loadtxt( ref_fol + '/force_file', dtype=float )
        run_force = np.loadtxt( run_fol + '/force_file', dtype=float )
        ref_force_abs = np.linalg.norm(ref_force)
        run_force_abs = np.linalg.norm(run_force)
        diff_force_abs = ref_force_abs - run_force_abs
        ## force difference per atom 
        diff_force_ele, diff_force_max, force1, force2 = get_diff_atom_force( ref_poscar.sites, run_poscar.sites, ref_force, run_force)
        print('diff energy=%.4f, force_mag=%.4f, force_rmse=%.4f' % (diff_energy, diff_force_abs, diff_force_ele ) )
        force_ele1 = np.append(force_ele1, force1)
        force_ele2 = np.append(force_ele2, force2)
        data.append([ref_energy/num_atom, run_energy/num_atom, diff_energy/num_atom, 
            ref_force_abs, run_force_abs, diff_force_abs, diff_force_ele, diff_force_max ])
        data_str.append(' %s %s %s\t %s %s %s %s %s'% (ref_energy, run_energy, diff_energy, 
            ref_force_abs, run_force_abs, diff_force_abs, diff_force_ele, diff_force_max ) )

data_str = '\n'.join(data_str) + '\n'
with open('diff_energy_force.dat', 'w') as f:
    f.write(data_str)

plot_force_type = 1 # 0 for magnitude, 1 for element

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

#fig, (ax1, ax2 ) = plt.subplots(nrows=1, ncols=2,figsize=(6,3))
fig, (ax1, ax2, ax3 ) = plt.subplots(nrows=1, ncols=3,figsize=(8,3))

data=np.array(data)
## plot figure 1 for energy
x=data[:,0] # energy of ab initio reference
y=data[:,1] # energy of MLFF run
xmin=x.min()
xmax=x.max()
single_line = np.array([xmin, xmax])
## plot a reference line
ax1.plot(single_line, single_line, 
        color='black', )
ax1.scatter(x, y,
        s=5, color='red',
        )


if plot_force_type == 0:
    x=data[:,3] # force of ab initio reference
    y=data[:,4] # force of MLFF run
else:
    x=force_ele1
    y=force_ele2
xmin=x.min()
xmax=x.max()
single_line = np.array([xmin, xmax])
## plot a reference line
ax2.plot(single_line, single_line, 
        color='black')
ax2.scatter(x, y,
        s=5, color='tab:blue',
        )

x=stress_ele1
y=stress_ele2
xmin=x.min()
xmax=x.max()
single_line = np.array([xmin, xmax])
## plot a reference line
ax3.plot(single_line, single_line, 
        color='black')
ax3.scatter(x, y,
        s=5, color='tab:green',
        )




ax1.set_xlabel('E(ab-init)/atom [eV]')
ax1.set_ylabel('E(MLFF)/atom [eV]')
ax2.set_xlabel('F(ab-init) [eV/A]')
ax2.set_ylabel('F(MLFF) [eV/A]')
ax3.set_xlabel('stress(ab-init) [kB]')
ax3.set_ylabel('stress(MLFF) [kB]')


def set_locators(ax):
    import matplotlib.ticker as tck
    #ax.yaxis.set_major_locator(tck.AutoLocator())
    ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.tick_params(
            top=True,bottom=True,right=True,left=True,
            direction='in',width=1.2)
    ax.tick_params(which='minor',
            top=True,bottom=True,right=True,left=True,
            direction='in',width=1.0)
    ax.grid(True)

set_locators(ax1)
set_locators(ax2)
set_locators(ax3)

plt.tight_layout()
figname='diff_mlff.png'
plt.savefig(figname)
plt.close()
print('%s is saved' % (figname))


