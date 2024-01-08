#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  SSCHA_exercise_PhaseTransition.py
#
# Import the cellconstructor stuff
import cellconstructor as CC
import cellconstructor.Phonons
import cellconstructor.ForceTensor
import cellconstructor.Structure
import cellconstructor.Spectral

# Import the modules of the force field
import fforces as ff
import fforces.Calculator

# Import the modules to run the sscha
import sscha, sscha.Ensemble, sscha.SchaMinimizer
import sscha.Relax, sscha.Utilities

import spglib
from ase.visualize import view

# Import Matplotlib to plot
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
import timeit

#Setting the variables:
#Setting the temperature in Kelvin:
Temperature = 0
#Setting the number of configurations:
configurations = 50
#Setting the names and location of the files:
Files_dyn_SnTe = "ffield_dynq"
#Set the number of irreducible q (reated to the supercell size):
nqirr = 3
#Setting the frequencies output file:
File_frequencies = "frequencies.dat"
#Setting the dynamical matrix output filename:
File_final_dyn = "final_sscha_T{}_".format(int(Temperature))
sobol = False
sobol_scatter = False

# Load the dynamical matrix for the force field
ff_dyn = CC.Phonons.Phonons("ffield_dynq", 3)

# Setup the forcefield with the correct parameters
ff_calculator = ff.Calculator.ToyModelCalculator(ff_dyn)
ff_calculator.type_cal = "pbtex"
ff_calculator.p3 = 0.036475
ff_calculator.p4 = -0.022
ff_calculator.p4x = -0.014
# Define the temperatures, from 50 to 300 K, 6 temperatures
temperatures = np.linspace(50, 300, 6)

lowest_hessian_mode = []
lowest_sscha_mode = []

# Perform a simulation at each temperature
t_old = Temperature

for Temperature in temperatures:
    # Load the starting dynamical matrix
    dyn = CC.Phonons.Phonons(File_final_dyn.format(int(t_old)), nqirr)

    # Prepare the ensemble
    ensemble = sscha.Ensemble.Ensemble(dyn, T0 = Temperature,
                                  supercell = dyn.GetSupercell())

    # Prepare the minimizer
    minim = sscha.SchaMinimizer.SSCHA_Minimizer(ensemble)
    minim.min_step_struc = 0.05
    minim.min_step_dyn = 0.002
    minim.kong_liu_ratio = 0.5
    minim.meaningful_factor = 0.000001
    #minim.root_representation = "root4"
    #minim.precond_dyn = False
    #minim.minim_struct = True
    #minim.neglect_symmetries = True
    minim.enforce_sum_rule = True  # Lorenzo's solution to the error

    # Prepare the relaxer (through many population)
    relax = sscha.Relax.SSCHA(minim, ase_calculator = ff_calculator,
                  N_configs=configurations, max_pop=20)

    # Relax
    relax.relax(sobol = sobol, sobol_scramble = sobol_scatter)
    #relax.relax()

    # Save the dynamical matrix
    relax.minim.dyn.save_qe(File_final_dyn.format(int(Temperature)))

    # Detect space group
    symm=spglib.get_spacegroup(relax.minim.dyn.structure.get_ase_atoms(),
                                      0.005)
    print('Current SG = ', symm,' at T=',int(Temperature))

    # Recompute the ensemble for the hessian calculation
    ensemble = sscha.Ensemble.Ensemble(relax.minim.dyn, T0 = Temperature,
                            supercell = dyn.GetSupercell())
    ensemble.generate(configurations, sobol = sobol, sobol_scramble = sobol_scatter)
    ensemble.get_energy_forces(ff_calculator, compute_stress = False)
    #gets the energies and forces from ff_calculator

    #update weights!!!
    ensemble.update_weights(relax.minim.dyn, Temperature)
    # Get the free energy hessian
    dyn_hessian = ensemble.get_free_energy_hessian(include_v4 = False)
    #free energy hessian as in Bianco paper 2017
    dyn_hessian.save_qe("hessian_T{}_".format(int(Temperature)))

    # Get the lowest frequencies for the sscha and the free energy hessian
    w_sscha, pols_sscha = relax.minim.dyn.DiagonalizeSupercell() #dynamical matrix
    # Get the structure in the supercell
    superstructure = relax.minim.dyn.structure.generate_supercell(relax.minim.dyn.GetSupercell())

    # Discard the acoustic modes
    acoustic_modes = CC.Methods.get_translations(pols_sscha, superstructure.get_masses_array())
    w_sscha = w_sscha[~acoustic_modes]

    lowest_sscha_mode.append(np.min(w_sscha) * CC.Units.RY_TO_CM) # Convert from Ry to cm-1

    w_hessian, pols_hessian = dyn_hessian.DiagonalizeSupercell() #recomputed dyn for hessian
    # Discard the acoustic modes
    acoustic_modes = CC.Methods.get_translations(pols_hessian, superstructure.get_masses_array())
    w_hessian = w_hessian[~acoustic_modes]
    lowest_hessian_mode.append(np.min(w_hessian) * CC.Units.RY_TO_CM) # Convert from Ry to cm-1
    #print ("\n".join(["{:.4f} cm-1".format(w * CC.Units.RY_TO_CM) for w in pols_hessian]))
    #exit()

    t_old = Temperature
# We prepare now the file to save the results
freq_data = np.zeros( (len(temperatures), 3))
freq_data[:, 0] = temperatures
freq_data[:, 1] = lowest_sscha_mode
freq_data[:, 2] = lowest_hessian_mode

# Save results on file
np.savetxt("{}_hessian_vs_temperature.dat".format(configurations),
            freq_data, header = "T [K]; SSCHA mode [cm-1]; Free energy hessian [cm-1]")

hessian_data = np.loadtxt("{}_hessian_vs_temperature.dat".format(configurations))

plt.figure(dpi = 120)
plt.plot(hessian_data[:,0], hessian_data[:,1], label = "Min SCHA freq", marker = ">")
plt.plot(hessian_data[:,0], hessian_data[:,2], label = "Free energy curvature", marker = "o")
plt.axhline(0, 0, 1, color = "k", ls = "dotted") # Draw the zero
plt.xlabel("Temperature [K]")
plt.ylabel("Frequency [cm-1]")
plt.legend()
plt.tight_layout()
plt.savefig('{}_Temp_Freq.png'.format(configurations))
#plt.show()

plt.figure(dpi = 120)
plt.plot(hessian_data[:,0], np.sign(hessian_data[:,2]) * hessian_data[:,2]**2,
                    label = "Free energy curvature", marker = "o")
plt.axhline(0, 0, 1, color = "k", ls = "dotted") # Draw the zero
plt.xlabel("Temperature [K]")
plt.ylabel("$\omega^2$ [cm-2]")
plt.legend()
plt.tight_layout()
plt.savefig('{}_Temp_Omeg.png'.format(configurations))
#plt.show()
