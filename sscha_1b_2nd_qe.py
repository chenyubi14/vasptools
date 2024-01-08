#!/usr/bin/env python
import cellconstructor as CC, cellconstructor.Phonons

# Import the numerical libraries and those for plotting
import numpy as np
import matplotlib.pyplot as plt

import sys, os
import argparse

print('Need QE dynamic matrices')
parser = argparse.ArgumentParser(description='Plot a single phonon plot with QE format')
parser.add_argument('--qe_file', type=str, default='harm_dyn',
        help='QE dynamic files start with the same string, like start_sscha')
args = parser.parse_args()
DYN_file_str = args.qe_file

all_files = os.listdir('.')
NQIRR = len([ fil for fil in all_files if fil[:len(DYN_file_str)] == DYN_file_str ])
if NQIRR == 0:
    print('Error! Dynamic files %s not found. Use --qe_file to specify' % DYN_file_str )
    sys.exit()

mater_harmonic_dyn = CC.Phonons.Phonons(DYN_file_str, NQIRR)

print('2nd order can be successfully read')

mater_harmonic_dyn.Symmetrize()
mater_harmonic_dyn.ForcePositiveDefinite()
mater_harmonic_dyn.Symmetrize()
mater_harmonic_dyn.save_qe("start_sscha")

print('Remove imaginary freq for sscha initial condition')


