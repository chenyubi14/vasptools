from pymatgen.io.vasp.outputs import BSVasprun, Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter
import matplotlib.pyplot as plt 

print('Need to run inside a bs_non-self subfolder')


## (1) use vasprun to process data
v = BSVasprun('vasprun.xml')
bs = v.get_band_structure(kpoints_filename="KPOINTS",line_mode=True)
print('Fermi level in bs_non-self/ OUTCAR is ', bs.efermi) # print the fermi level
parent_efermi=Vasprun("../vasprun.xml").efermi
print('efermi in scf is %s' % (parent_efermi) )
bs.efermi=parent_efermi
print('shift Fermi level to be consistent as %.5f' % (bs.efermi) )


## (2) use plotter to get the plot
bsplot = BSPlotter(bs)
ticks=bsplot.get_ticks()
print(ticks)
# if want to show legends, change in this bs_labels. Use ax_legend() to omit showing this label
zero_to_efermi=True
bsplot.get_plot(ylim=(-6, 10), zero_to_efermi=zero_to_efermi) #, bs_labels=['wBeO']


## (3) add some plot features
ax = plt.gca()
#### omit showing legend
ax.get_legend().remove()
#### remove legend with this command
#ax.legend(fontsize=16)#, loc="upper left") # remove legend like 'band 0 up' # will get 'No handles with labels found to put in legend.'
#### labels of x and y axis
ax.set_xlabel('Wave Vector',rotation=0)
ax.set_ylabel('', rotation=0) # energies (eV) '$\mathrm{E}-\mathrm{E}_\mathrm{VBM} (eV)$' ,rotation=270
#### put y labels on the right
#ax.yaxis.set_label_position('right') # position of labels
#ax.yaxis.tick_right() # put the ticks on the right hand side
#### tick size
plt.yticks(fontsize=22)
#### set title
ax.set_title("Band Structure", fontsize=25)
#### Draw a horizontal line to represent fermi level when zero_to_efermi=True
if zero_to_efermi:
	xlim = ax.get_xlim()
	ax.hlines(0, xlim[0], xlim[1], linestyles="dashed", color="black")


ylimit=None
####################################
#### HERE!!! set ylimit
ylimit=[-5,9] ## Material project
#ylimit=[-1,0.5] ##cd3as2
#ylimit=[-2,1] ## BaMnSb2
#ylimit=[-1.5,1.5] ## 135
#ylimit=[-5,2]
####################################
if ylimit:
    plt.ylim(ylimit)


## (4) save the plot
plt.tight_layout()
plt.savefig('bandstructure.pdf',dpi=600)
print('figure generated: bandstructure.pdf')
#bsplot.save_plot('bandstructure.png')

