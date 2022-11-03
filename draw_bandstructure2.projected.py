import pymatgen as mg
from pymatgen.io.vasp.outputs import BSVasprun, Vasprun
from pymatgen import Spin
from pymatgen.electronic_structure.plotter import BSPlotterProjected #, BSDOSPlotter, DosPlotter
import matplotlib.pyplot as plt 

#run = BSVasprun("vasprun.xml", parse_projected_eigen=True)
#bs = run.get_band_structure("KPOINTS")

## (1) use vasprun to process data
v = BSVasprun('vasprun.xml',parse_projected_eigen=True)
bs = v.get_band_structure(kpoints_filename="KPOINTS",line_mode=True)
print(bs.efermi) # print the fermi level

## (2) use plotter to get the plot
bsplot = BSPlotterProjected(bs)
ticks=bsplot.get_ticks()
print(ticks)
# if want to show legends, change in this bs_labels. Use ax_legend() to omit showing this label
zero_to_efermi=True
#bsplot.get_elt_projected_plots(ylim=(-5, 20), zero_to_efermi=zero_to_efermi) #, bs_labels=['wBeO']
bsplot.get_elt_projected_plots_color(zero_to_efermi=zero_to_efermi,ylim=[-5,20]) #, bs_labels=['wBeO']

## (3) add some plot features
ax = plt.gca()
# labels of x and y axis
ax.set_xlabel('Wave Vector',rotation=0)
ax.set_ylabel('', rotation=0) # energies (eV) '$\mathrm{E}-\mathrm{E}_\mathrm{VBM} (eV)$' ,rotation=270
# put y labels on the right
ax.yaxis.set_label_position('right') # position of labels
ax.yaxis.tick_right() # put the ticks on the right hand side
plt.yticks(fontsize=22)
# set title
ax.set_title("wurtzite BeO Band Structure", fontsize=25)
# Draw a horizontal line to represent fermi level when zero_to_efermi=True
if zero_to_efermi:
	xlim = ax.get_xlim()
	ax.hlines(0, xlim[0], xlim[1], linestyles="dashed", color="black")
# remove legend with this command
#ax.legend(fontsize=16)#, loc="upper left") # remove legend like 'band 0 up' # will get 'No handles with labels found to put in legend.'

# (4) save the plot
plt.tight_layout()
plt.savefig('bandstructure.pdf',dpi=600)
print('figure generated: bandstructure.pdf')
#bsplot.save_plot('bandstructure.png')

