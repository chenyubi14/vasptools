import ase, ase.visualize
import cellconstructor as CC, cellconstructor.Structure
import sys, os

## read file name, qe format
qe_filename = sys.argv[1] ## scf_population.dat
## save file name, vasp format
vasp_filename = sys.argv[2]

struct = CC.Structure.Structure()
struct.read_scf(qe_filename)
ase_struct = struct.get_ase_atoms()
ase_struct.write(vasp_filename)
