from pymatgen.io.vasp.outputs import BSVasprun
from pymatgen.electronic_structure.plotter import BSPlotterProjected 
import matplotlib.pyplot as plt 

print('Need to run inside a bs_non-self subfolder')


## (1) use vasprun to process data
v = BSVasprun('vasprun.xml',parse_projected_eigen=True)
bs = v.get_band_structure(kpoints_filename="KPOINTS",line_mode=True)
print('Fermi level in OUTCAR is ', bs.efermi) # print the fermi level

#################################################################################################
dictio={'Cd':['s','p'],'As':['s','p']} # for Cd3As2
#### set ylimit
ylimits=[-1.0,1.0] # HERE!!! ylim cd3as2
#ylimits=[-2.0,1.0] # HERE!!! ylim BaMnSb2
zero_to_efermi=True
#################################################################################################


## (2) use plotter to get the plot
bsplot = BSPlotterProjected(bs) # Note this is necessary
ticks=bsplot.get_ticks()
print(ticks)
# if want to show legends, change in this bs_labels. Use ax_legend() to omit showing this label
#################################################################################################
# Here are two types of BS plots
### type1:
bsplot.get_projected_plots_dots(dictio,zero_to_efermi=zero_to_efermi,ylim=ylimits) 
### type2:
#bsplot.get_projected_plots_dots_patom_pmorb(dictio,dictpa,zero_to_efermi=zero_to_efermi,ylim=ylimits)
#################################################################################################


## (3) add some plot features
ax = plt.gca()
#### omit showing legend
#ax.get_legend().remove()
#### remove legend with this command
#ax.legend(fontsize=16)#, loc="upper left") # remove legend like 'band 0 up' # will get 'No handles with labels found to put in legend.'
#### labels of x and y axis
#ax.set_xlabel('Wave Vector',rotation=0)
ax.set_ylabel('', rotation=0) # energies (eV) '$\mathrm{E}-\mathrm{E}_\mathrm{VBM} (eV)$' ,rotation=270
#### put y labels on the right
#ax.yaxis.set_label_position('right') # position of labels
#ax.yaxis.tick_right() # put the ticks on the right hand side
#plt.yticks(fontsize=22)
#### set title
#ax.set_title("Band Structure", fontsize=25)
#### Draw a horizontal line to represent fermi level when zero_to_efermi=True
if zero_to_efermi:
	xlim = ax.get_xlim()
	ax.hlines(0, xlim[0], xlim[1], linestyles="dashed", color="black")

ax.set_ylim(ylimits)



# (4) save the plot
plt.tight_layout()
plt.savefig('bandstructure.png',dpi=600,transparent=True)
print('figure generated: bandstructure.png')
#bsplot.save_plot('bandstructure.png')

