#!/usr/bin/env python
# Import the sscha code
import sscha, sscha.Ensemble, sscha.SchaMinimizer, sscha.Relax, sscha.Utilities

import cellconstructor as CC, cellconstructor.Phonons
import cellconstructor.Structure, cellconstructor.calculators

import numpy as np
import sys, os
import argparse

sys.path.append(os.environ['SCRIPT'])
from sscha_calc_vasp import get_calculator

print('Run VASP jobs in current folder, will keep rewriting VASP files')

### Parse arguments
parser = argparse.ArgumentParser(description='sscha minimize at a single temperature')
parser.add_argument('temperature', type=float, default=300,
        help='Supercell size for phonon dispersion')
parser.add_argument('--num_conf', type=int, default=50,
        help='Number of configurations in a population')
parser.add_argument('--num_popu', type=int, default=20,
        help='Maximum number of populations (iterations)')
parser.add_argument('--kgrid', type=int, nargs=3, default=[1,1,1],
        help='kgrid for VASP calculator')
parser.add_argument('--min_step', type=float, default=None,
        help='Minimization step, default=1, smaller if too slow')
args = parser.parse_args()
TEMPERATURE = args.temperature
N_CONFIGS = args.num_conf
MAX_ITERATIONS = args.num_popu
kgrid = args.kgrid
min_step = args.min_step
DYN_file_str = 'start_sscha'

all_files = os.listdir('.')
NQIRR = len([ fil for fil in all_files if fil[:len(DYN_file_str)] == DYN_file_str ])
if NQIRR == 0:
    print('Error! Dynamic files %s not found. Should run sscha_1a or 1b' % DYN_file_str )
    sys.exit()


'''
Relaxation needs
    calculator
    minimizer -> ensemble -> dynamic_mat
'''


### calculator
## Get Vasp calculator
calculator = get_calculator(kgrid=kgrid, incar='INCAR')

### minimizer -> ensemble -> dynamic_mat
## Let us load the starting dynamical matrix without imaginary frequencies
mater_harmonic_dyn = CC.Phonons.Phonons(DYN_file_str, NQIRR)
# Initialize the random ionic ensemble
ensemble = sscha.Ensemble.Ensemble(mater_harmonic_dyn, TEMPERATURE)
# Initialize the free energy minimizer
minim = sscha.SchaMinimizer.SSCHA_Minimizer(ensemble)
print('\tThe minimization step is automatically found.  \
However, if the relaxation is too slow, set_minimization_step 0.1 or 0.01')
if min_step:
    minim.set_minimization_step(min_step)

# Initialize the NVT simulation
relax = sscha.Relax.SSCHA(minim, calculator, N_configs = N_CONFIGS,
        max_pop = MAX_ITERATIONS)

# Define the I/O operations
# To save info about the free energy minimization after each step
ioinfo = sscha.Utilities.IOInfo()
ioinfo.SetupSaving("minim_info")
relax.setup_custom_functions(custom_function_post = ioinfo.CFP_SaveAll)


# Run the NVT simulation (save the stress to compute the pressure)
#relax.relax(get_stress = True)
relax.vc_relax(target_press = 0)
## NVT ensemble with variable lattice parameters (cell shape)
#relax.vc_relax(fix_volume = True)

# If instead you want to run a NPT simulation, use
# The target pressure is given in GPa.
#relax.vc_relax(target_press = 0)

# You can also run a mixed simulation (NVT) but with variable lattice parameters
#relax.vc_relax(fix_volume = True)

# Now we can save the final dynamical matrix
# And print in stdout the info about the minimization
relax.minim.finalize()
relax.minim.dyn.save_qe("sscha_T{}K_dyn".format(TEMPERATURE))

