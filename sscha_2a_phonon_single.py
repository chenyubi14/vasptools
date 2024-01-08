# Import the CellConstructor library to plot the dispersion
import cellconstructor as CC, cellconstructor.Phonons
import cellconstructor.ForceTensor

# Import the numerical libraries and those for plotting
import numpy as np
import matplotlib.pyplot as plt

import sys, os
import argparse

'''
For the error:
    Error, the imaginary part of the real space force constant
    is not zero
Edit img_thr:
(1) /home/yubi/repo/miniconda3/envs/sscha/lib/python3.10/site-packages/cellconstructor/ForceTensor.py:196
    img_thr=1e-6 -> img_thr=1e-5
    super_dyn = current_dyn.GenerateSupercellDyn(phonons.GetSupercell(), img_thr  =1e-5)
(2) /home/yubi/repo/miniconda3/envs/sscha/lib/python3.10/site-packages/cellconstructor/Phonons.py:1888
    GenerateSupercellDyn(self, supercell_size, img_thr = 1e-5)
'''

parser = argparse.ArgumentParser(description='Plot a single phonon plot with QE format')
parser.add_argument('--qe_file', type=str, default='start_sscha',
        help='QE dynamic files start with the same string, like start_sscha')
parser.add_argument('--label', type=str, default='T-K',
        help='label of the plot')
parser.add_argument('--unit', type=str, choices=['thz','cm'], default='thz',
        help='Unit of phonon dispersion')
### start_sscha is the positive 0K phonon

args = parser.parse_args()
DYN_file_str = args.qe_file
label = args.label
unit = args.unit


### Total number of q points
N_POINTS = 1000

### Get PATH in the brilluin zone from band.conf
fileinfo2 = 'band.conf'
dict2 = np.loadtxt( fileinfo2, comments='#',dtype=str, delimiter='=') # this one works for data with several columns.
dict2 = [ [dict2[i,0].strip(),dict2[i,1].strip()] for i in range(len(dict2)) ]
dict2 = dict(dict2)
## '$\\Gamma$ X W K $\\Gamma$ L U W L', replace \Gamma with G
PATH = dict2['BAND_LABELS'].replace('$\\Gamma$', 'G')
## 'G X W K G L U W L', remove the spaces
qlabels=PATH.split() ## ['G', 'X', ...]
PATH = PATH.replace(' ', '')
## PATH="GXWXKGL"
### Get q point definition
## Remove comma
qpoints = dict2['BAND'].replace(',','')
qpoints = np.reshape( qpoints.split(), (-1,3) )
qpoints = qpoints.astype(float)
assert len(PATH) == len(qpoints), 'Error! Number of band labels and qpoints do not match in %s' % (fileinfo2)


## Here save position of the special points as dictionary
SPECIAL_POINTS = {}
for i, q in enumerate(qlabels):
    print(q, qpoints[i])
    SPECIAL_POINTS[q] = qpoints[i]
#SPECIAL_POINTS = {"G": [0,0,0],
#                  "X": [0, .5, .5],
#                  "L": [.5, .5, .5],
#                  "W": [.25, .75, .5],
#                  "K": [3/8., 3/4., 3/8.]}



# The number of irreducible q points
# i.e., the number of files in which the phonons are stored
## count the number of files harmonic_dyn1, harmonic_dyn2, ...
all_files = os.listdir('.')
NQIRR = len([ fil for fil in all_files if fil[:len(DYN_file_str)] == DYN_file_str ])
if NQIRR == 0:
    print('Error! Dynamic files %s not found. Use --qe_file to specify' % DYN_file_str )
    sys.exit()


# --------------------- THE SCRIPT FOLLOWS ---------------------

# Load phonons from DYN_file_str
mater_dyn = CC.Phonons.Phonons(DYN_file_str, NQIRR)


# Get the band path
qpath, data = CC.Methods.get_bandpath(
                mater_dyn.structure.unit_cell,
                PATH,
                SPECIAL_POINTS,
                N_POINTS)
xaxis, xticks, xlabels = data # Info to plot correclty the x axis
## Get the phonon dispersion along the path
## Rydberg is converted to cm-1 automatically
mater_dispersion = CC.ForceTensor.get_phonons_in_qpath(mater_dyn, qpath)

if unit.lower() == 'thz':
    ## convert cm-1 to THz
    mater_dispersion = mater_dispersion * 0.030
    ylabel = 'Phonon Frequency [THz]'
elif unit.lower() == 'cm':
    ## already in cm-1 unit
    ylabel = 'Phonon Frequency [$\mathrm{cm^{-1}}$]'

SMALL_SIZE = 16
MEDIUM_SIZE = 18
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title

plt.figure()
ax = plt.gca()

nmodes = mater_dyn.structure.N_atoms * 3
for i in range(nmodes):
    lbl=None
    if i == 0:
        lbl = label
    ax.plot(xaxis, mater_dispersion[:,i], label=lbl,
            color='royalblue'
            #color='k', ls='dashed',
            )

## Plot zero frequency horizontal line
ax.axhline(0, 0, 1, 
        color = 'k', ls = 'dashed', lw = 0.4)
# Plot vertical lines for each high symmetry points
for x in xticks:
    ax.axvline(x, 0, 1, 
            color = "k", lw = 0.4, ls='dashed')

# Set the x labels to the high symmetry points
ax.set_xticks(xticks)
ax.set_xticklabels(xlabels)
#ax.set_xlabel("Q path")
ax.set_ylabel(ylabel)


ax.legend()

plt.tight_layout()
figname = "sscha_dispersion_%s.png" % (label)
plt.savefig(figname)
print('%s is generated' % (figname) )
#plt.show()
