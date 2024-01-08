import cellconstructor as CC, cellconstructor.Phonons
import spglib
import argparse
import os
import sys

parser = argparse.ArgumentParser(description='Generate population at a single temperature')
parser.add_argument('qe_file', type=str,default='dyn_start_population',
        help='string for QE dynamic files, like start_sscha')
parser.add_argument('--symprec', type=float,default=1e-5,
        help='symmetry precision')

args = parser.parse_args()
DYN_file_str = args.qe_file
symprec = args.symprec

### Obtain NQIRR
all_files = os.listdir('.')
NQIRR = len([ fil for fil in all_files if fil[:len(DYN_file_str)] == DYN_file_str ])
if NQIRR == 0:
    print('Error! Dynamic files %s not found. argument = (T qe_file) to specify' % DYN_file_str )
    sys.exit()

dyn = CC.Phonons.Phonons(DYN_file_str, nqirr=NQIRR)
symm1 = spglib.get_spacegroup(dyn.structure.get_ase_atoms(), symprec = symprec)
print('For %s, space group is %s \n' % (DYN_file_str, symm1) )


#symm2=spglib.get_pointgroup(dyn.structure.get_ase_atoms() )
symm2 = CC.symmetries.QE_Symmetry(dyn.structure)
symm2.threshold=1e-8
print('threshold_default=%s' % (symm2.threshold))

## need this GetSymmetries to initialize
symm2.SetupQPoint(verbose = True)
#symm2.GetSymmetries() ## actually not working
#symm2.QE_nsym
#symm2.PrintSymmetries()
print('QE symmetry2 is %s' % (symm2.QE_nsym) )

symm3 = CC.symmetries.QE_Symmetry(dyn.structure)
symm3.SetupFromSPGLIB()
print('\nQE symmetry3 is %s' % (symm3.QE_nsym) )
