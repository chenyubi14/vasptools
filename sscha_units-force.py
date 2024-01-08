#!/usr/bin/env python
import numpy as np
import sys

## Convert vasp force unit (eV/Angstrom) into QE energy unit (Ry/Bohr)
## 1eV/Angstrom = 0.03889379346800142 Ry/Bohr
## Ry/Bohr = 25.71104309541616 eV/Angstrom

## unit conversion from http://greif.geo.berkeley.edu/~driver/conversions.html

filename = sys.argv[1]
data = np.loadtxt(filename, dtype=float)
data = data / 25.71104309541616
np.savetxt(filename, data)
