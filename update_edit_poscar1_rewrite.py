# In[1]:
import numpy as np
import sys
from pathlib import Path
from pymatgen.core import Structure

# Goal: use Pymatgen to save POSCAR
# Could add selective dynamics


print('Could enter sel for selective_dynamics. Enter atom indices like 0 1 2 3 ... to fix those atoms')


atoms = []
# In[3]:
if len(sys.argv) > 1:
    if sys.argv[1] == 'sel':
        print('Turn on selective dynamics.')
        sel = True
        atoms = sys.argv[2:]
else:
    sel = False

#parser = argparse.ArgumentParser(description='POSCAR perturb atoms')
#parser.add_argument('--sel', type=int, nargs='*',choices=range(0, len(sites)), default=False
#        help='Enter the selected atoms to perturb its neighbors')
#args = parser.parse_args()
#sel=args.sel # a list of atoms to perturb

def struc2poscar(struc, folder, fname='POSCAR', selective_dynamics=False, atoms=[]):
    folder = Path(folder)
    speinfo=struc.composition.get_el_amt_dict()
    spe1num=speinfo[struc[0].specie.name] 
    if selective_dynamics:
        selective_dynamics = [[True]*3]*(struc.num_sites)
        for atom in atoms:
            atom = int(atom)
            selective_dynamics[atom] = [False]*3
            print('fix atom%s %s%s %s' % (atom, struc[atom].specie.name, int(atom % spe1num ), struc[atom].frac_coords))
        print('Python indices: index + 1 to match with Vesta')
        struc.to(filename=str(folder/fname), selective_dynamics=selective_dynamics, fmt='poscar')
    else:
        struc.to(filename=str(folder/fname), fmt='poscar')


print('Read POSCAR...')
poscar='POSCAR'
structpos=Structure.from_file(poscar)
folder = './'
struc2poscar(structpos, folder, fname='POSCAR',selective_dynamics=sel, atoms=atoms)



