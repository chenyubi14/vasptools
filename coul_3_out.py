import numpy as np
import os
import sys
import argparse
from pymatgen.core import Structure
from pymatgen.io.vasp.outputs import Outcar
sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import find_files, read_incar

### process arguments
poscar = 'POSCAR'
struc=Structure.from_file(poscar)
parser = argparse.ArgumentParser(description='Output U parameter')
parser.add_argument('orbital', type=str, nargs=1, choices=['d','f'],
        help='d or f orbital that is being tested')
parser.add_argument('ind', type=int, nargs=1, choices=range(0, len(struc)),
        help='Enter the index of atom that has shifted potential, need to start from 0',)
args = parser.parse_args()
ind = args.ind[0]
orb = args.orbital[0]




### find folders
sc=find_files('.',header='sc',var='coul',remove=False)
nsc=find_files('.',header='nsc',var='coul',remove=False)
assert len(sc) == len(nsc), 'Error! nsc/sc number of folders are not the same'
num = len(sc)

### read charge from each folder
outcar_ref = Outcar('coul_reference/OUTCAR')
ch_ref = outcar_ref.charge[ind]
print('Reference=\n    %s\n' % ch_ref  )
ch_ref = ch_ref[orb]

shifts = []
dns = []
dn0s = []
for i in range(num):
    ## read potential shift from INCAR
    dict_incar = read_incar(sc[i], incar='INCAR')
    poten_shift = dict_incar['LDAUU']
    poten_shift = np.float64(poten_shift.split())
    poten_shift = poten_shift[np.where(poten_shift!=0.0)] #[0]
    if len(poten_shift) == 0:
        poten_shift = 0
    else:
        poten_shift = poten_shift[0]
    ## read charge from OUTCAR
    outcar_sc = Outcar(sc[i]+ '/OUTCAR')
    ch1 = outcar_sc.charge[ind]
    outcar_nsc = Outcar(nsc[i]+ '/OUTCAR')
    ch2 = outcar_nsc.charge[ind]
    print('i=%s shift=%s\n sc=%s\nnsc=%s' % (i, poten_shift, ch1, ch2))
    shifts.append(poten_shift)
    dns.append( ch1[orb] - ch_ref )
    dn0s.append( ch2[orb] - ch_ref )
    ## Chi_ii
    #chi1 = (ch1[orb] - ch_ref ) / poten_shift
    #chi2 = (ch2[orb] - ch_ref ) / poten_shift
    #paraU = 1/chi1 - 1/chi2
    print('shift=%s, dn=%.3f, dn0=%.3f \n' % (poten_shift, ch1[orb] - ch_ref, ch2[orb] - ch_ref) )

### Plot the U parameter
coeff = np.polyfit(shifts,dns,1)
coeff0 = np.polyfit(shifts,dn0s,1)
f1 = np.poly1d(coeff)
f2 = np.poly1d(coeff0)
chi = np.round(coeff[0],5)
chi0 = np.round(coeff0[0],5)
paraU = 1/chi - 1/chi0
print('chi=%.5f, chi0=%.5f, U=%.5f' % (chi, chi0, paraU) )

import matplotlib.pyplot as plt 
SMALL_SIZE = 16
MEDIUM_SIZE = 18
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
plt.figure()
plt.plot(shifts, dns, label='dn, chi=%s'%chi,
        marker='o',linewidth=3, color='tab:blue')
plt.plot(shifts, f1(shifts), label='fit dn',
        linestyle='--',linewidth=3, color='blue')
plt.plot(shifts, dn0s, label='dn0, chi0=%s'%chi0 ,
        marker='o',linewidth=3,color='tab:red')
plt.plot(shifts, f2(shifts), label='fit dn0',
        linestyle='--',linewidth=3, color='red')

plt.legend()
plt.title('U=1/chi-1/chi0=%.5f' % (paraU) )
plt.xlabel('Potential Shift [eV]')
plt.ylabel('Charge change')
plt.tight_layout()

figname='calc_U.pdf'
plt.savefig(figname,dpi=600, transparent=True)
plt.close()

