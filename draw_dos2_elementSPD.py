import pymatgen as mg
from pymatgen.io.vasp.outputs import BSVasprun, Vasprun
from pymatgen.electronic_structure.plotter import DosPlotter
import sys
import matplotlib.pyplot as plt 
from pymatgen.core.periodic_table import Element

if len(sys.argv) < 2:
	print('Error! Enter the element that you want its DOS, like Cu,Zn,O,Be')
	sys.exit()
else:
	elements_name = sys.argv[1]
#elements_name='Cu'
el=Element(elements_name)

removeticks=False # if True, remove x ticks to stack a DOS vertically with a BS
dosrun = Vasprun("vasprun.xml", parse_dos=True)
dos = dosrun.complete_dos
#print(dosrun.efermi)

print(dos.efermi)
dosplot = DosPlotter(sigma=0.1) # choose smearing here. Can make the plot more smooth.
#dosplot.add_dos("Total DOS", dos) # get total density of states
dosplot.add_dos_dict(dos.get_element_spd_dos(el)) # get dos of each element
plt = dosplot.get_plot() #zero_to_efermi=True
plt.grid()
ax = plt.gca()

if removeticks:
	plt.xticks([]) # remove ticks on x axis
ax.set_ylabel('',rotation=270) # 'Density of states'
ax.set_xlabel('',rotation=0)
ax.yaxis.set_label_position('right')
#ax.yaxis.tick_right()

#ax.set_xlim([-10,10])
#plt.xlim((-30,30))
plt.ylim((-10,10))
#plt.ylim((-.005,0.005))
#plt.ylim((0,0.2))
plt.tight_layout()
figurename = 'dos_%s_bulk.pdf' % elements_name
plt.savefig(figurename,dpi=600)
print('figure generated: %s' % figurename )
#plt.show()