#!/usr/bin/env python
from pymatgen.core import Structure
import sys
import os
from pathlib import Path


fol=Path(sys.argv[1])
allfiles = os.listdir(fol)
#print(allfiles)

for fil_i in allfiles:
    fil_i = str(Path(fol) / fil_i)
    if os.path.isdir(fil_i):
        print(fil_i)
        poscar = str( fil_i + '/POSCAR')
        struc=Structure.from_file(poscar)
        struc.sort() 
        struc.to(filename=poscar, fmt='poscar')
