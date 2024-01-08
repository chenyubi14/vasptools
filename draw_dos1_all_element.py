import pymatgen as mg
from pymatgen.io.vasp.outputs import BSVasprun, Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter, BSDOSPlotter, DosPlotter
from pymatgen.core import Structure
import sys
import matplotlib.pyplot as plt 

removeticks=False # if True, remove x ticks to stack a DOS vertically with a BS
dosrun = Vasprun("vasprun.xml", parse_dos=True)
dos = dosrun.complete_dos
#print(dosrun.efermi)
print('efermi in dos=', dos.efermi)
########################################
parent_efermi=Vasprun("../vasprun.xml").efermi
#parent_efermi=Vasprun("../bs_non-self.6/vasprun.xml").efermi
print('efermi in scf is %s' % (parent_efermi) )
dos.efermi=parent_efermi
print('shift Fermi level to scf efermi')
########################################
#dosplot = DosPlotter(sigma=0.1) # choose smearing here. Can make the plot more smooth.
dosplot = DosPlotter() # choose smearing here. Can make the plot more smooth.
dosplot.add_dos("Total DOS", dos) # get total density of states
dosplot.add_dos_dict(dos.get_element_dos()) # get dos of each element
plt = dosplot.get_plot()
plt.grid()
ax = plt.gca()
if removeticks:
	plt.xticks([]) # remove ticks on x axis
ax.set_ylabel('',rotation=270) # 'Density of states'
ax.set_xlabel('',rotation=0)
ax.yaxis.set_label_position('right')
#ax.yaxis.tick_right()
#ax.set_xlim([-5,5])
#ax.set_xlim([-2,1])
ax.set_xlim([-1,0.5])
#plt.ylim((-30,30))
plt.ylim((0,30))
plt.tight_layout()
figurename = 'dos_all_element.pdf'
plt.savefig(figurename,dpi=600)
print('figure generated: %s' % figurename)
#plt.show()