import pymatgen
from pymatgen.io.vasp.outputs import BSVasprun, Vasprun
from pymatgen.electronic_structure.plotter import DosPlotter
from pymatgen.core import Structure
import sys
import matplotlib.pyplot as plt 
from pymatgen.core.periodic_table import Element

print('Need LORBIT=11 to have the projected charge density!!')

if len(sys.argv) < 2:
	print('Error! Enter the NLINE of the atom that you want, starting from NLINE=10')
	sys.exit()
else:
	atom_index = int(sys.argv[1]) - 10 # convert NLINE to pymatgen index

stru=Structure.from_file('CONTCAR')
atom = stru[atom_index]

removeticks=False # if True, remove x ticks to stack a DOS vertically with a BS
dosrun = Vasprun("vasprun.xml", parse_dos=True)
dos = dosrun.complete_dos
#print(dosrun.efermi)

print(dos.efermi)
dosplot = DosPlotter(sigma=0.1) # choose smearing here. Can make the plot more smooth.
#dosplot.add_dos("Total DOS", dos) # get total density of states
dosplot.add_dos_dict(dos.get_site_spd_dos(atom)) # get dos of each element
plt = dosplot.get_plot() #zero_to_efermi=True
plt.grid()
ax = plt.gca()

if removeticks:
	plt.xticks([]) # remove ticks on x axis
ax.set_ylabel('',rotation=270) # 'Density of states'
ax.set_xlabel('',rotation=0)
ax.yaxis.set_label_position('right')
#ax.yaxis.tick_right()

#plt.xlim((-30,30))
ax.set_xlim([-10,18])
#plt.ylim((-0.2,0.2))
#plt.ylim((-.005,0.005))
#plt.ylim((0,0.2))
plt.tight_layout()
figurename = 'dos_atom%s_bulk.pdf' % atom_index
plt.savefig(figurename,dpi=600)
print('figure generated: %s' % figurename )
#plt.show()
