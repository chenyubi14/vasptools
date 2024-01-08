#!/usr/bin/env python
import numpy as np
import sys

## Convert vasp energy unit (eV) into QE energy unit (Ry)
## 1eV = 0.0734985857 Ry
## 1Ry = 13.605698 eV

filename = sys.argv[1]
data = np.loadtxt(filename, dtype=float)
data = data  / 13.605698
np.savetxt(filename, data)
