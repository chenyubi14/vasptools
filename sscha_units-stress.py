#!/usr/bin/env python
import numpy as np
import sys

## Convert vasp stress unit (kB) into QE energy unit (Ry/Bohr^3)
## 1kB = 1 kilobar = 0.1GPa
## 1 Ha/Bohr^3 = 2 Ry/Bohr^3 = 29421.02648438959 GPa = 294210.2648438959 kB
## 1 Ry/Bohr^3 = 147105.13242194796 kB
## 1kB = 6.797859350900532e-06 Ry/Borh^3

## units from http://greif.geo.berkeley.edu/~driver/conversions.html

filename = sys.argv[1]
data = np.loadtxt(filename, dtype=float)
data = data / 147105.13242194796
np.savetxt(filename, data)
