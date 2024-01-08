#!/usr/bin/env python
import cellconstructor as CC, cellconstructor.Phonons
import sscha, sscha.Ensemble
import numpy as np
import sys, os
import argparse

sys.path.append(os.environ['SCRIPT'])
from sscha_calc_vasp import get_calculator


print('Generate ensemble for the new population')


### Parse arguments
parser = argparse.ArgumentParser(description='Generate population at a single temperature')
parser.add_argument('temperature', type=float, default=300,
        help='temperature for calculation')
parser.add_argument('population', type=int,
        help='population number')
parser.add_argument('--qe_file', type=str, default=None,
        help='string for QE dynamic files, like start_sscha')
parser.add_argument('--num_conf', type=int, default=50,
        help='Number of configurations in a population')
#parser.add_argument('--num_popu', type=int, default=100,
#        help='Maximum number of populations (iterations), Only used in relax')
#parser.add_argument('--kgrid', type=int, nargs=3, default=[1,1,1],
#        help='kgrid for VASP calculator')
parser.add_argument('--min_step', type=float, default=None,
        help='Minimization step, default=1, smaller if too slow')
parser.add_argument('--direc', type=str, default='population',
        help='directory to save ensemble data for each population')
parser.add_argument('--sobol', type=bool, default=False, choices=[1, 0],
        help='use less num_conf=5000 not 10000 if Sobol sequence, a better mapping of gaussian than random')
args = parser.parse_args()
TEMPERATURE = args.temperature
DYN_file_str = args.qe_file
N_CONFIGS = args.num_conf
#MAX_ITERATIONS = args.num_popu
#kgrid = args.kgrid
min_step = args.min_step
save_directory = args.direc
population_id = args.population
sobol = bool(args.sobol)

## Set DYN_file_str based on population_id 
if DYN_file_str is None:
    if population_id == 1:
        DYN_file_str = 'start_sscha'
    else:
        ## Get dynamic matrix from population-1
        DYN_file_str = 'dyn_end_population{}_'.format(population_id-1)
save_directory = save_directory + str(population_id)
print('Read dyn %s, save in folder %s' % (DYN_file_str, save_directory) )

### Obtain NQIRR
all_files = os.listdir('.')
NQIRR = len([ fil for fil in all_files if fil[:len(DYN_file_str)] == DYN_file_str ])
if NQIRR == 0:
    print('Error! Dynamic files %s not found. argument --qe_file to specify' % DYN_file_str )
    sys.exit()


print('num_conf=%s sobol=%s' % (N_CONFIGS, sobol))
## Load the dynamical matrix
dyn = CC.Phonons.Phonons(DYN_file_str, nqirr=NQIRR)
## Prepare the ensemble
ensemble = sscha.Ensemble.Ensemble(dyn, TEMPERATURE)
## Generate the ensemble, not minimizer
ensemble.generate(N_CONFIGS, sobol=sobol)
print('saving ensemble...')
# Save the ensemble into a directory
ensemble.save(save_directory, population_id)


