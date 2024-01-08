import ase, ase.visualize
#from ase.build.tools import sort
import cellconstructor as CC, cellconstructor.Structure
import sys, os
import argparse
from path import Path

parser = argparse.ArgumentParser(description='Convert scf_population.dat to POSCAR')
parser.add_argument('fol_from', type=str,
        help='Read scf_population.dat files in fol_from')
parser.add_argument('fol_to', type=str,
        help='Save POSCAR files in fol_to')

args = parser.parse_args()
fol_from = args.fol_from
fol_to = args.fol_to

os.listdir(fol_from)
files = []
indices = []
for fil in os.listdir(fol_from):
    if 'scf_population' in fil:
        ind = fil.split('_')[-1].split('.dat')[0]
        #files.append( fil )
        #indices.append(ind)
        ## indices may not follow the order [1,2,3...]

        ## read file name, qe format
        qe_filename = str( Path(fol_from) / fil ) ## scf_population.dat
        ## save file name, vasp format
        vasp_filename = str( Path(fol_to) / 'POSCAR'+ind )

        struct = CC.Structure.Structure()
        struct.read_scf(qe_filename)
        ase_struct = struct.get_ase_atoms()
        ## should not sort here!!!
        ##ase_struct = sort(ase_struct)
        ase_struct.write(vasp_filename)
