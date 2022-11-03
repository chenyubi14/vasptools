import pymatgen as mg
from pymatgen.io.vasp.outputs import BSVasprun, Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter, BSDOSPlotter, DosPlotter
import matplotlib.pyplot as plt 
import numpy as np

SMALL_SIZE = 20
MEDIUM_SIZE = 24
BIGGER_SIZE = 26
plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title

# plot density of states and bandstructure
dosrun = Vasprun("dos_non-self/vasprun.xml", parse_dos=True)
dos = dosrun.complete_dos
#bsrun = BSVasprun("bs_non-self/vasprun.xml", parse_projected_eigen=True) # Plot colored bandstructure
bsrun = BSVasprun("bs_non-self/vasprun.xml") # Plot colored bandstructure
#bsrun = BSVasprun("bs_non-self/vasprun.xml",bs_legend=None) # No color and no colorbar in bandstructure
bs = bsrun.get_band_structure(kpoints_filename="bs_non-self/KPOINTS",line_mode=True)
print('dos_fermi=%s,bs_fermi=%s' % (dos.efermi, bs.efermi))

#bsdosplot = BSDOSPlotter(cb_energy_range=6,vb_energy_range=6,sigma=0.1,egrid_interval=2,fig_size=(9,7),legend_fontsize=17,dos_legend='center right')
bsdosplot = BSDOSPlotter(cb_energy_range=9,vb_energy_range=4,egrid_interval=2,fig_size=(9,7),legend_fontsize=17,dos_legend='center right')
plt=bsdosplot.get_plot(bs,dos=dos)
#plt=bsdosplot.get_plot(bs)

## save
plt.tight_layout()
plt.savefig('unitbulk_bs_dos.pdf',dpi=600)
print('figure generated: unitbulk_bs_dos.pdf')

#plt.suptitle('wurtzite BeO',fontsize=22)

## plot density of states
#fig=plt.figure()
#dosrun = Vasprun("dos_non-self/vasprun.xml", parse_dos=True)
#dos = dosrun.complete_dos
##print(dos.efermi) #print(dosrun.efermi)
#dosplot = DosPlotter(sigma=0.1)
#dosplot.add_dos("Total DOS", dos)
#dosplot.add_dos_dict(dos.get_element_dos())
#plt = dosplot.get_plot() # this is the plot function
#plt.grid()
#ax1 = plt.gca()
#ax1.set_xlim([-5,20])
#plt.xticks([])
#ax1.set_ylabel('',rotation=270) # 'Density of states'
#ax1.set_xlabel('',rotation=0)
#ax1.yaxis.set_label_position('right')
#ax1.set_title("Density of states")
##ax.yaxis.tick_right()
##plt.xlim((-30,30))
##plt.ylim((-50,50))
##plt.savefig('dos_w%s.%se.png' % (sys.argv[1],sys.argv[2]) )


## plot bandstructure
#ax2=fig.add_subplot(1,2,2)
#ax2.plot([1,2,3],[3,1,2])
##run = BSVasprun("vasprun.xml", parse_projected_eigen=True)
##bs = run.get_band_structure("KPOINTS")
#v = BSVasprun('bs_non-self/vasprun.xml')
#bs = v.get_band_structure(kpoints_filename="KPOINTS",line_mode=True)
#bsplot = BSPlotter(bs)
## get the plot
#ticks=bsplot.get_ticks()
#print(ticks)
#bsplot.get_plot(ylim=(-5, 20), zero_to_efermi=True, bs_labels=['bb'])
#print(bs.efermi, dos.efermi)
## add some features
##ax = plt.gca()
#
## labels of x and y axis
#ax2.set_xlabel('Wave Vector',rotation=0)
#ax2.set_ylabel('energies (eV)', rotation=0) # '$\mathrm{E}-\mathrm{E}_\mathrm{VBM} (eV)$' ,rotation=270
## put y labels on the right
#ax2.yaxis.set_label_position('right') # position of labels
#ax2.yaxis.tick_right() # put the ticks on the right hand side
##ax2.set_yticks(fontsize=22) #plt.yticks(fontsize=22) 
#plt.yticks(fontsize=22) 
#ax2.set_title("wurtzite BeO Band Structure", fontsize=25)
#xlim = ax2.get_xlim()
#ax2.hlines(0, xlim[0], xlim[1], linestyles="dashed", color="black")
##plt.legend('wurtzite BeO')
## add legend
##ax.plot((), (), "b-", label="wurtzite")
##ax.plot((), (), "r--", label="wurtzite")
##ax.legend(fontsize=16)#, loc="upper left")
##ax.plot((), (), label="wurtzite")
#ax2.legend(fontsize=16)#, loc="upper left")
##ax.legend(loc='upper center', shadow=True, fontsize='x-large')



