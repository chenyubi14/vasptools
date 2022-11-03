import sys
import os
import numpy as np
import matplotlib.pyplot as plt
sys.path.append(os.environ['SCRIPT'])
path = os.environ['PWD'] + '/'

from class0_functions1 import find_files, savedata
from class1_read import read_file_values

class drawsingleinfo:
    def __init__(self, x_str, y_str, header, lattice_structure=None, xlim=None, ylim=None):
        # fixed info for all... the format of infodict(information dictionary) is 'keyword': ('xlabel', 'unit')
        self.legend = ['s', 'p', 'd']
        self.yfile_infodict = {'DOS': 'vasprun.xml', 'PDOS': 'vasprun.xml'}
        self.xlabel_infodict = {'energy':('energy','eV')}
        self.ylabel_infodict = {'DOS':('DOS', ''), 'PDOS':('PDOS','')}
        # plot info provided
        self.lattice_structure = lattice_structure
        self.header = header
        self.x_str = x_str
        self.y_str = y_str
        # plot info generated
        self.x_file_open = self.yfile_infodict[x_str]
        self.xlabel = '%s %s' % self.xlabel_infodict[x_str]
        self.ylabel = '%s %s' % self.ylabel_infodict[y_str]
        self.title = '%s(%s)' % (x_str, y_str)
        self.xlim=xlim
        self.ylim=ylim
        # plot data initial
        self.xx=np.array([])
        self.data=np.array([])
        # read data
        self.read()
    
    def dos_pdos(self):
        return 0

    def bs(self):
        return 0

