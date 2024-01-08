#!/usr/bin/env python
import sscha, sscha.Ensemble, sscha.SchaMinimizer
import sscha.Relax, sscha.Utilities
import cellconstructor as CC, cellconstructor.Phonons

import argparse
import os, sys
import spglib

# Import Matplotlib to plot
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
import timeit

print('usage: python $SCRIPT/sscha_4e* 300 100 10 --direc population.hessian10 ')

### Parse arguments
parser = argparse.ArgumentParser(description='Generate population at a single temperature')
parser.add_argument('temperature1', type=float, default=300,
        help='temperature of ensemble')
parser.add_argument('temperature2', type=float, default=300,
        help='target temperature')
parser.add_argument('population', type=int,
        help='population number')
parser.add_argument('--qe_file', type=str,default='dyn_start_population',
        help='dyn_start_population files for old_T hessian')
#parser.add_argument('--num_popu', type=int, default=100,
#        help='Maximum number of populations (iterations), Only used in relax')
parser.add_argument('--min_step', type=float, default=None,
        help='Minimization step, default=1, smaller if too slow')
parser.add_argument('--direc', type=str, default='population.hessian',
        help='ensemble data (eg. population.hessian10) for old_T ')
parser.add_argument('--include_v4', type=bool, default=False,
        help='include 4th order correction')
args = parser.parse_args()
temperature1 = args.temperature1 #in Kelvin
temperature2 = args.temperature2 #in Kelvin
DYN_file_str = args.qe_file
POPULATION = args.population
#MAX_ITERATIONS = args.num_popu
min_step = args.min_step
load_dir = args.direc
include_v4 = args.include_v4

if DYN_file_str == 'dyn_start_population':
    DYN_file_str = 'dyn_start_population{}_'.format(POPULATION)
### Obtain NQIRR
all_files = os.listdir('.')
NQIRR = len([ fil for fil in all_files if fil[:len(DYN_file_str)] == DYN_file_str ])
if NQIRR == 0:
    print('Error! Dynamic files %s not found. argument --qe_file to specify' % DYN_file_str )
    sys.exit()

## Get number of configurations
all_files = os.listdir(load_dir)
find_str = 'forces_population' + str(POPULATION)
num_conf = len([ fil for fil in all_files if fil[:len(find_str)] == find_str ])
print('Number of configurations is %s ' % num_conf )



# The SSCHA dynamical matrix is needed (the one after convergence)
# We reload the final result (no need to rerun the sscha minimization)
dyn_sscha_final = CC.Phonons.Phonons(DYN_file_str, nqirr=NQIRR)
# We reset the ensemble
print('Ensemble temperature is ', temperature1)
ensemble = sscha.Ensemble.Ensemble(dyn_sscha_final, T0 = temperature1,
                    supercell = dyn_sscha_final.GetSupercell())
ensemble.load(load_dir, population = POPULATION, N = num_conf)
print("Updating the weights at temperature ", temperature2)
ensemble.update_weights(dyn_sscha_final, temperature2)
# If the sscha matrix was not the one used to compute the ensemble
# We must update the ensemble weights
# We can also use this function to simulate a different temperature.
# Initialize the minimizer and perform the free energy minimization
minim = sscha.SchaMinimizer.SSCHA_Minimizer(ensemble)
minim.meaningful_factor = 0.001
#minim.min_step_struc = 0.05
#minim.min_step_dyn = 0.002
minim.kong_liu_ratio = 0.2
#minim.root_representation = "root4"
#minim.precond_dyn = False
#minim.minim_struct = True
#minim.neglect_symmetries = True
minim.enforce_sum_rule = True
minim.init()

# Save the minimization details
ioinfo = sscha.Utilities.IOInfo()
ioinfo.SetupSaving("minim_{}".format(POPULATION))

minim.run(custom_function_post = ioinfo.CFP_SaveAll)
minim.finalize()
minim.dyn.save_qe("dyn_end_population{}_".format(POPULATION))
print('Dynamics saved as dyn_end_population%s_' % POPULATION )

# ----------- COMPUTE THE FREE ENERGY HESSIAN -----------
print("Compute the free energy hessian, rather than minimizing. 4th order correction is %s" % include_v4 )
dyn_hessian = minim.ensemble.get_free_energy_hessian(include_v4 = include_v4)
#dyn_hessian = ensemble.get_free_energy_hessian(include_v4 = True,
#              get_full_hessian = True,verbose = True) # Full calculus
# We can save the free energy hessian as a dynamical matrix in QE format
dyn_hessian.save_qe("hessian_")
#dyn_hessian.save_qe("dyn_end_population{}_".format(POPULATION))
print('Saved as hessian_'.format(POPULATION))

