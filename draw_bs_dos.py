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
bsrun = BSVasprun("bs_non-self/vasprun.xml", parse_projected_eigen=True) # Plot colored bandstructure
#bsrun = BSVasprun("bs_non-self/vasprun.xml") # Plot not colored bandstructure
#bsrun = BSVasprun("bs_non-self/vasprun.xml",bs_legend=None) # No color and no colorbar in bandstructure
bs = bsrun.get_band_structure(kpoints_filename="bs_non-self/KPOINTS",line_mode=True)
print('dos_fermi=%s,bs_fermi=%s' % (dos.efermi, bs.efermi))


########################################
parent_efermi=Vasprun("vasprun.xml").efermi
print('efermi in scf is %s' % (parent_efermi) )
bs.efermi=parent_efermi
dos.efermi=bs.efermi
print('shift Fermi level to be consistent as %.5f' % (dos.efermi) )
########################################


bsdosplot = BSDOSPlotter(
        egrid_interval=0.5,
        #egrid_interval=2,
        #cb_energy_range=1,vb_energy_range=1,
        #cb_energy_range=1.5,vb_energy_range=1.5,
        cb_energy_range=8,vb_energy_range=4,
        fig_size=(10,5),
        legend_fontsize=17,tick_fontsize=15,
        #legend_fontsize=17,tick_fontsize=17,
        #bs_legend='center',
        bs_legend='upper left',
        dos_legend='center',
        ) 
# supported bs_legend: 'upper right', 'upper left', 'lower left', 'lower right', 'right', 'center left', 'center right', 'lower center', 'upper center', 'center'
bs_ax, dos_ax=bsdosplot.get_plot(bs,dos=dos)

#ylimits=[-5,9]
ylimits=[-1.5,1.5]
bs_ax.set_ylim(ylimits)
dos_ax.set_ylim(ylimits)

## save
#plt.savefig('unitbulk_bs_dos.pdf',dpi=600,transparent=True)
plt.savefig('unitbulk_bs_dos.png',dpi=600)
print('figure generated: unitbulk_bs_dos')

