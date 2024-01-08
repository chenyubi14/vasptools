from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import DosPlotter
from pymatgen.core import Structure
from pymatgen.electronic_structure.core import Orbital
import sys
import matplotlib.pyplot as plt 
from pymatgen.core.periodic_table import Element

print('Need LORBIT=11 to have the projected charge density!! Plot for one orbital (s,px,py,pz,dx2,dxy,...) of an atom')

if len(sys.argv) < 3:
	print('Error! Enter $1=index of selected atom, $2=orbital. index=0 for the first atom. Orbitals: s=0, py=1, pz=2, px=3, dxy=4, dyz=5, dz2=6, dxz=7, dx2=8, ...')
	sys.exit()
else:
	atom_index = int(sys.argv[1]) # convert NLINE to pymatgen index
	orbitals=[]
	for i in range(len(sys.argv)-2):
		orbitals.append(Orbital(int(sys.argv[2+i])))

stru=Structure.from_file('CONTCAR')
atom = stru[atom_index]

removeticks=False # if True, remove x ticks to stack a DOS vertically with a BS
dosrun = Vasprun("vasprun.xml", parse_dos=True)
dos = dosrun.complete_dos
#print(dosrun.efermi)

print(dos.efermi)
dosplot = DosPlotter(sigma=0.1) # choose smearing here. Can make the plot more smooth.
dos_dict={}
for orb in orbitals:
	print('%s' % (orb.name))
	dos_dict[orb]=dos.get_site_orbital_dos(atom, orbital=orb)
dosplot.add_dos_dict(dos_dict) # get dos of each element
plt = dosplot.get_plot() #zero_to_efermi=True
plt.grid()
ax = plt.gca()

if removeticks:
	plt.xticks([]) # remove ticks on x axis
ax.set_ylabel('',rotation=270) # 'Density of states'
ax.set_xlabel('',rotation=0)
ax.yaxis.set_label_position('right')
#ax.yaxis.tick_right()

ax.set_xlim([-5,5])
#ax.set_xlim([-3,1])
plt.tight_layout()
figurename = 'dos_atom%s_orbitals.pdf' % (atom_index)
plt.savefig(figurename,dpi=600)
print('figure generated: %s' % figurename )
