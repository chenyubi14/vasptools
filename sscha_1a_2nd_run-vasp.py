#!/usr/bin/env python
import cellconstructor as CC, cellconstructor.Phonons
import cellconstructor.Structure, cellconstructor.calculators

import numpy as np
import sys, os
import argparse

sys.path.append(os.environ['SCRIPT'])
from sscha_calc_vasp import get_calculator


### Parse arguments
parser = argparse.ArgumentParser(description='phonon calculation with finite disp method')
parser.add_argument('supersize', type=int, nargs=3, default=[1,1,1],
        help='Supercell size for phonon dispersion')
parser.add_argument('--kgrid', type=int, nargs=3, default=[1,1,1],
        help='K grid sampling for finite displacement')
parser.add_argument('--f_stru', type=str, nargs=1,default='POSCAR',
        help='File name of the unit cell POSCAR')
parser.add_argument('--incar', type=str, nargs=1, default='INCAR',
        help='Read INCAR parameters from INCAR file or use the default manual setup')
parser.add_argument('--relaxed', type=bool, nargs=1, default=True,
        help='The POSCAR is relaxed or not')
parser.add_argument('--save_qe',type=bool, nargs=1, default=True,
        help='save format to be multiple qe dynamic files')
args = parser.parse_args()
f_stru = args.f_stru
kgrid = args.kgrid
supersize = args.supersize
isrelaxed = args.relaxed
save_qe = args.save_qe
incar = args.incar



### Get material structure
mater_structure = CC.Structure.Structure()
mater_structure.read_generic_file(f_stru)
### Get VASP calculator
calculator = get_calculator(kgrid, incar=incar)
### Get relaxation method
relax = CC.calculators.Relax(mater_structure, calculator)



## Relax the unrelaxed POSCAR structure 
if isrelaxed:
    print('Assume %s has been relaxed' % f_stru)
else:
    mater_structure = relax.static_relax()


''' 
### Edit source code
### first, make finite displacement continue from the unfinished job
(1) cellconstructor/Phonons.py: in function "compute_phonons_finite_displacements_sym" 
    after "ase_calculator.directory = "disp_{}".format(3*i + j)"
    before "energy, forces = calculators.get_energy_forces(ase_calculator, s)" 
    add these lines
         try:
             ase_calculator.read()
             print('read the finished folder %s ' % (ase_calculator.directory))
         except:
             print('Folder unfinished, run %s' % (ase_calculator.directory))
### second, problem of precision in phonon plotting
(2) ase/calculators/vasp/vasp.py: change tolerance in function "check_state" from 1e-15 to 1e-7 
        position tolerance should be higher to properly plot phonon for some cases
        "def check_state(self, atoms, tol=1e-7):"
ase/calculators/vasp/vasp.py: In read(), it automatically considered directory, so not changed
'''
### Use finite displacement to calculate 0K phonon
print('If it takes too long to find Number of symmetry inequivalent displacements, check POSCAR')
mater_harmonic_dyn = CC.Phonons.compute_phonons_finite_displacements(
            mater_structure, 
            calculator, 
            supercell = tuple(supersize)
            )
# Impose the symmetries
mater_harmonic_dyn.Symmetrize()
### Save phonon in qe format, might have imaginary frequencies
if save_qe:
    # save the dynamical matrix in the quantum espresso format
    mater_harmonic_dyn.save_qe("harm_dyn")
else:
    mater_harmonic_dyn.save_phonopy()
    print('Saved FORCE_CONSTANTS')
    print('Note phonopy.yaml will not be generated, so load_phonopy will not work')



## If the dynamical matrix has imaginary frequencies, remove them
print('Initial condition should not have imaginary freq, remove them')
mater_harmonic_dyn.ForcePositiveDefinite()
mater_harmonic_dyn.Symmetrize()
## save the dynamical matrix in the quantum espresso format
if save_qe:
    mater_harmonic_dyn.save_qe("start_sscha")


