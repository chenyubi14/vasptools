#!/usr/bin/env python
import cellconstructor as CC, cellconstructor.Phonons
import sscha, sscha.Ensemble
import numpy as np
import sys, os
import argparse

sys.path.append(os.environ['SCRIPT'])
from sscha_calc_vasp import get_calculator
from sscha_cluster_expanse import configure_cluster

### Parse arguments
parser = argparse.ArgumentParser(description='Generate population at a single temperature')
parser.add_argument('temperature', type=float, default=300,
        help='Supercell size for phonon dispersion')
parser.add_argument('population', type=int,
        help='population number')
parser.add_argument('--qe_file', type=str, default=None,
        help='string for QE dynamic files, like start_sscha')
parser.add_argument('--num_conf', type=int, default=50,
        help='Number of configurations in a population')
#parser.add_argument('--num_popu', type=int, default=100,
#        help='Maximum number of populations (iterations), Only used in relax')
parser.add_argument('--kgrid', type=int, nargs=3, default=[1,1,1],
        help='kgrid for VASP calculator')
#parser.add_argument('--min_step', type=float, default=None,
#        help='Minimization step, default=1, smaller if too slow')
parser.add_argument('--direc', type=str, default='population',
        help='directory to save ensemble data for each population')
parser.add_argument('--mater', type=str, default='mat_work',
        help='material name used in cluster directory')
parser.add_argument('--ncores', type=str, default=4 ,
        help='number of cores in job script')
args = parser.parse_args()
TEMPERATURE = args.temperature
DYN_file_str = args.qe_file
N_CONFIGS = args.num_conf
#MAX_ITERATIONS = args.num_popu
kgrid = args.kgrid
#min_step = args.min_step
save_directory = args.direc
population_id = args.population
dirname = args.mater
ncores = args.ncores

## Set DYN_file_str based on population_id 
if DYN_file_str is None:
    if population_id == 1:
        DYN_file_str = 'start_sscha'
    else:
        ## Get dynamic matrix from population-1
        DYN_file_str = 'dyn_end_population{}_'.format(population_id-1)
save_directory = save_directory + str(population_id)

### Obtain NQIRR
all_files = os.listdir('.')
NQIRR = len([ fil for fil in all_files if fil[:len(DYN_file_str)] == DYN_file_str ])
if NQIRR == 0:
    print('Error! Dynamic files %s not found. argument --qe_file to specify' % DYN_file_str )
    sys.exit()


print('Read the script for bug: AttributeError: Vasp object has no attribute copy')
print('No actually, it can only be used for QE, not for VASP, give up on this one')

''' A bug in ensemble.compute_ensemble
It assumes calculator is cellconstructor_calculator, because 
             # Create a local copy of the calculator for each thread, to avoid conflicting modifications
             calculators = [cellconstructor_calc.copy() for i in range(0, len(jobs))]
Since VASP is ase_calculator, this line does not work
Can solve it by edit /home/yubi/repo/miniconda3/envs/sscha/lib/python3.10/site-packages/sscha/Cluster.py 
             if isinstance(cellconstructor_calc, list):
                 calculators = cellconstructor_calc
             else:
                 calculators = [cellconstructor_calc.copy() for i in range(0, len(jobs))]
Now, pass multiple Vasp calculators as a list
'''

## Load the dynamical matrix
dyn = CC.Phonons.Phonons(DYN_file_str, nqirr=NQIRR)
ensemble = sscha.Ensemble.Ensemble(dyn, TEMPERATURE)
ensemble.generate(N_CONFIGS)

''' A single calculator
# Get the espresso and cluster configurations
calculator = get_calculator(kgrid)
cluster_config = configure_cluster(dirname=dirname, ncores=ncores)
# Compute the ensemble
ensemble.compute_ensemble(calculator, cluster=cluster_config)

Below is to pass multiple calculators
'''
# Get the espresso and cluster configurations
calculators = [get_calculator(kgrid) for i in range(N_CONFIGS) ]
cluster_config = configure_cluster(dirname=dirname, ncores=ncores)
# Compute the ensemble
ensemble.compute_ensemble(calculators, cluster=cluster_config)


# Save the ensemble (using population 2 to avoid overwriting the other one)
ensemble.save(save_directory, population_id)


