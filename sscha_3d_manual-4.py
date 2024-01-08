#!/usr/bin/env python
import sscha, sscha.Ensemble, sscha.SchaMinimizer
import sscha.Utilities
import cellconstructor as CC, cellconstructor.Phonons
import argparse
import os, sys
import spglib

parser = argparse.ArgumentParser(description='Generate population at a single temperature')
parser.add_argument('temperature', type=float, default=300,
        help='temperature for calculation')
parser.add_argument('population', type=int,
        help='population number')
parser.add_argument('--qe_file', type=str,default='dyn_start_population',
        help='string for QE dynamic files, like start_sscha')
args = parser.parse_args()
TEMPERATURE = args.temperature
DYN_file_str = args.qe_file
POPULATION = args.population
load_dir = 'population' + str(POPULATION)

if DYN_file_str == 'dyn_start_population':
    DYN_file_str = 'dyn_start_population{}_'.format(POPULATION)
### Obtain NQIRR
all_files = os.listdir('.')
NQIRR = len([ fil for fil in all_files if fil[:len(DYN_file_str)] == DYN_file_str ])
if NQIRR == 0:
    print('Error! Dynamic files %s not found. argument = (T qe_file) to specify' % DYN_file_str )
    sys.exit()

## Get number of configurations
all_files = os.listdir(load_dir)
find_str = 'forces_population' + str(POPULATION)
num_conf = len([ fil for fil in all_files if fil[:len(find_str)] == find_str ])
print('Number of configurations is %s ' % num_conf )



print('Loading ensemble data...')
dyn = CC.Phonons.Phonons(DYN_file_str, nqirr=NQIRR)
ensemble = sscha.Ensemble.Ensemble(dyn, TEMPERATURE)
ensemble.load(load_dir, population = POPULATION, N = num_conf)
minim = sscha.SchaMinimizer.SSCHA_Minimizer(ensemble)
## Now we setup the minimization parameters
## Since we are quite far from the correct solution,
## we will use a small optimization step
#minim.set_minimization_step(0.25)

## Reduce the threshold for the gradient convergence
minim.meaningful_factor = 0.001
#minim.min_step_struc = 0.05
#minim.min_step_dyn = 0.002
#minim.kong_liu_ratio = 0.5 
#minim.root_representation = "root4"
#minim.precond_dyn = False
#minim.minim_struct = True
#minim.neglect_symmetries = True
#minim.use_spglib = True ## need this for the correct symmetry
minim.enforce_sum_rule = True  # Lorenzo's solution to the error


#minim.init()
minim.init(verbosity = True )


# Save the minimization details
ioinfo = sscha.Utilities.IOInfo()
ioinfo.SetupSaving("minim_{}".format(POPULATION))


minim.run(custom_function_post = ioinfo.CFP_SaveAll)
minim.finalize()
minim.dyn.save_qe("dyn_end_population{}_".format(POPULATION))
print('Dynamics saved as dyn_end_population%s_' % POPULATION )


symm=spglib.get_spacegroup(minim.dyn.structure.get_ase_atoms(),
        1e-5)
print('Symmetry group = ', symm)
