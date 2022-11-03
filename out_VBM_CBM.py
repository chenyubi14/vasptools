import numpy as np
import sys
import os
sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import read_incar
from pymatgen.io.vasp.outputs import BSVasprun, Vasprun
from pathlib import Path
from class1_read import read_file_values

# Get VBM CBM that uses the potential alignment

f1=Path('/work2/07884/tg871834/stampede2/berylliumO/wurtzite_01_super_perfect/perfect_run_aexx0.405')
f2=Path('/work2/07884/tg871834/stampede2/berylliumO/wurtzite_11_vO/defect2e')
def cbm_vbm(folder):
	folder = Path(folder)
	run = BSVasprun(folder/"vasprun.xml", parse_projected_eigen=True)
	bs = run.get_band_structure(folder/"KPOINTS")
	info1=bs.get_cbm() # there is the kpoint related to CBM
	# {'band_index': defaultdict(<class 'list'>, {<Spin.up: 1>: [192], <Spin.down: -1>: [192]}), 'kpoint_index': [0], 'kpoint': <pymatgen.electronic_structure.bandstructure.Kpoint>, 'energy': 13.286, 'projections': {}}
	info2=bs.get_vbm() # Valence band maximum
	energyCBM=np.round(info1['energy'],4) # keyword: energy
	energyVBM=np.round(info2['energy'],4)
	#print('%senergy=%s'%(['CBM','VBM'][c_v], energy))
	print('CBM=%s VBM=%s' % (energyCBM,energyVBM))
	return energyCBM,energyVBM

def read_C(folder):
	folder = Path(folder)
	with open(str(folder/'freysoldt_correction_ref-bulk/sx2.sh'),'r') as f:
		lines=f.readlines()[0]
	C=float(lines.split('-C')[1].split('> sx2.fc')[0])
	return C

# energy of CBM and VBM in perfect supercells
perfenergyCBM,perfenergyVBM = cbm_vbm(f1)
# calculate VBM of defect cell by potential alignment
#mydict=read_incar(f2, incar='DEFECT')
#freycorr=float(mydict['FREYCORRALL'])
C=read_C(f2) # defect minus perfect
# calculate CBM of defect cell by averaging
defectenergyVBM = C+perfenergyVBM
defectenergyCBM = defectenergyVBM + (perfenergyCBM-perfenergyVBM)
print('%s, %s' % (defectenergyVBM, defectenergyCBM))