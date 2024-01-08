#!/usr/bin/env python
import cellconstructor as CC, cellconstructor.Phonons
import cellconstructor.Structure, cellconstructor.calculators

import ase, ase.calculators
from ase.calculators.vasp import Vasp

import numpy as np, matplotlib.pyplot as plt
import sys, os

print('Note currently cellconstructor is not able to calculate phonon correctly. Do not trust this function')

print('Need phonopy.yaml and FORCE_CONSTANTS')

mater_harmonic_dyn = CC.Phonons.Phonons()
mater_harmonic_dyn.load_phonopy()

print('2nd order can be successfully read')

mater_harmonic_dyn.Symmetrize()
mater_harmonic_dyn.ForcePositiveDefinite()
mater_harmonic_dyn.Symmetrize()
mater_harmonic_dyn.save_qe("start_sscha")

print('Remove imaginary freq for sscha initial condition')


