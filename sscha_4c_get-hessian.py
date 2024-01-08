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

### Parse arguments
parser = argparse.ArgumentParser(description='Generate population at a single temperature')
parser.add_argument('temperature', type=float, default=300,
        help='temperature for calculation')
parser.add_argument('population', type=int,
        help='population number')
parser.add_argument('--qe_file', type=str,default='dyn_start_population',
        help='string for QE dynamic files, like start_sscha')
#parser.add_argument('--num_popu', type=int, default=100,
#        help='Maximum number of populations (iterations), Only used in relax')
parser.add_argument('--min_step', type=float, default=None,
        help='Minimization step, default=1, smaller if too slow')
parser.add_argument('--direc', type=str, default='population',
        help='directory to save ensemble data for each population')
parser.add_argument('--include_v4', type=bool, default=False,
        help='include 4th order correction')
args = parser.parse_args()
Temperature = args.temperature #in Kelvin
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
ensemble = sscha.Ensemble.Ensemble(dyn_sscha_final, T0 = Temperature,
                    supercell = dyn_sscha_final.GetSupercell())
ensemble.load(load_dir, population = POPULATION, N = num_conf)
print("Updating the weights...")
ensemble.update_weights(dyn_sscha_final, Temperature)
# If the sscha matrix was not the one used to compute the ensemble
# We must update the ensemble weights
# We can also use this function to simulate a different temperature.

# ----------- COMPUTE THE FREE ENERGY HESSIAN -----------
print("Compute the free energy hessian, rather than minimizing. 4th order correction is %s" % include_v4 )
dyn_hessian = ensemble.get_free_energy_hessian(include_v4 = include_v4)
#dyn_hessian = ensemble.get_free_energy_hessian(include_v4 = True,
#              get_full_hessian = True,verbose = True) # Full calculus
# We can save the free energy hessian as a dynamical matrix in QE format
dyn_hessian.save_qe("hessian_")
dyn_hessian.save_qe("dyn_end_population{}_".format(POPULATION))
print('Saved as hessian_ and dyn_end_population{}_'.format(POPULATION))

## -------------------------------------------------------
## We calculate the frequencies of the hessian:
#w_hessian, pols_hessian = dyn_hessian.DiagonalizeSupercell()
#
## Print all the frequency converting them into cm-1 (They are in Ry)
#print("\n".join(["{:16.4f} cm-1".format(w * CC.Units.RY_TO_CM) for w in w_hessian]))

