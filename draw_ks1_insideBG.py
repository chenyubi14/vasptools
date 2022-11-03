# In[0]: pre setting
import numpy as np
import sys 
import os
pwd = os.environ['PWD'] + '/'
#sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import read_incar
from pymatgen.io.vasp.outputs import BSVasprun, Vasprun, Eigenval
from pathlib import Path
#from class1_read import read_file_values
import matplotlib.pyplot as plt 
import math

SMALLER_SIZE=14
SMALL_SIZE = 15
MEDIUM_SIZE = 18
BIGGER_SIZE = 18

plt.rc('font', size=SMALLER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title


# In[1]: folder setting
if len(sys.argv) < 2:
        print('Error! Enter one argument: the name of perfect supercell folder, used to calculate CBM and VBM. Need the sampling to have CBM,VBM!! Like')
        print('${WORK1}/wurtzite_00_unit/unit.AEXX0.405\n${WORK5}/wurtzite_01_bulk/aexx0.36')
        print('\n Remember to edit DEFECTTYPE to set title')
        sys.exit()
f1=Path(sys.argv[1])
f2=Path(pwd)

mydict=read_incar(f2,incar='DEFECT')
defecttype = mydict['DEFECTTYPE']
print('defecttype=%s'%defecttype)

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
        print('perfect cell VBM=%s CBM=%s ' % (energyVBM,energyCBM))
        return energyCBM,energyVBM

def read_C(folder):
        folder = Path(folder)
        with open(str(folder/'freysoldt_correction_ref-bulk/sx2.sh'),'r') as f:
                lines=f.readlines()[0]
        C=float(lines.split('-C')[1].split('> sx2.fc')[0])
        return C

def read_eigenvalues(folder,defectenergyCBM,defectenergyVBM):
        eigen=Eigenval(folder/'EIGENVAL')
        eigen = eigen.eigenvalues # a dictionary of eigenvalues: {up:(1,Nbands,2), down:(1,Nbands,2)}
        spin_keys = list(eigen.keys()) # Not sure whether it is [up, down] or [down, up]
        spin_up_key = spin_keys[0].up  # '.up' avoids uncertainty: spin_key.up will become up key, spin_key.down will become down key
        spin_down_key = spin_keys[0].down # will give down key
        ksupstates = eigen[spin_up_key][0] # '[0]' makes it from (1,Nbands,2) to (Nbands,2)
        if spin_down_key in spin_keys:
                ksdownstates = eigen[spin_down_key][0]
        else:
                ksdownstates = ksupstates
        #ksdownstates = eigen[spin_down_key][0]
        # calculate spin of the defect
        ksupstatesocc = ksupstates[:,1]
        ksdownstatesocc = ksdownstates[:,1]
        spin = abs(np.sum(ksupstatesocc) - np.sum(ksdownstatesocc)) / 2
        
        # select out the states within the range of VBM and CBM
        graceperiodVBM = 0.1 # Plot only if both spin up and spin down states are above VBM+graceperiod and belove CBM-graceperiod
        graceperiodCBM = 0.1 # Plot only if both spin up and spin down states are above VBM+graceperiod and belove CBM-graceperiod
        selected = (defectenergyVBM+graceperiodVBM <ksupstates[:,0])*(ksupstates[:,0]<defectenergyCBM-graceperiodCBM) + (defectenergyVBM+graceperiodVBM<ksdownstates[:,0])*(ksdownstates[:,0]<defectenergyCBM-graceperiodCBM)
        ksupstates = ksupstates[ selected]
        ksdownstates = ksdownstates [ selected]        
        print('Up states to be plotted: \n%s' % ksupstates)
        print('Down states to be plotted: \n%s' % ksdownstates)
        list_selected = ' '.join(list( (np.where(selected==True)[0]+1).astype(str) ))
        print('\nUPDEFECTLEVEL=%s\nDOWNDEFECTLEVEL=%s\n'%(list_selected,list_selected))
        return ksupstates, ksdownstates, spin

def initialized_first_ks(x_dict,initial_mode=2):
        '''
        Initial_mode = 1: apply to case (1) in plot_ks
        Initial_mode = 2: apply to cases (2) in plot_ks

        return: the least number of levels to be plotted degeneratelly
        '''
        if initial_mode == 2:
                return np.max(list(x_dict.keys()))
        elif initial_mode == 1:
                return 1

def plot_panel_ks(ax, spin_states, maxx, maxy, defectenergyVBM, tol=0.5, initial_mode=2, updown=1):
        '''
        plot defect levels of a single panel, either spin up or spin down
        '''
        length = len(spin_states)
        i = 0
        if updown == 1: # spin up
                #x_dict = {1:[[1./6,2./6]], 2:[[1./18.,5./18],[5./18,8./18]]}
                x_dict = {1:[[3./14,4./14]], 2:[[1./14.,2./14],[5./14,6./14]],3:[[1./14,2./14],[3./14,4./14],[5./14,6./14]],4:[[1./48,5./48],[7./48,11./48],[13./48,17./48],[19./48,23./48]]}
        else: # spin down
                #x_dict = {1:[[11./16,13./16]], 2:[[9./16.,11./16],[13./16,15./16]]}
                x_dict = {1:[[10./14,11./14]], 2:[[8./14.,9./14],[12./14,13./14]],3:[[8./14,9./14],[10./14,11./14],[12./14,13./14]],4:[[25./48,29./48],[31./48,35./48],[37./48,41./48],[43./48,47./48]]}
        first_ks = initialized_first_ks(x_dict,initial_mode=initial_mode) # is plotting the first several KS levels. Plot levels in order from left to right
        # After plotting the first several, set first_ks=0. The value of first_ks is initialized to be the maximum keyword in x_dict
        while i < length: # still have length-i levels to plot
                ksenergy = spin_states[i,0] #ksenergyocc
                degenerate = max(1,first_ks) # initially first_ks=4, will plot 4 levels degenerately. Later first_ks=0, degenerate will be 1
                for j in range(i+1,length):
                        if math.isclose(spin_states[j,0],ksenergy,abs_tol=tol):
                                degenerate += 1
                if degenerate >= 4:
                        degenerate = min(4,length-i) 
                xx = np.array(x_dict[degenerate])
                for j in range(degenerate):
                        ksenergy = spin_states[i+j,0]
                        ksenergy = ksenergy - defectenergyVBM
                        yy=np.array([ksenergy, ksenergy])
                        meanx=xx[j].mean()
                        ax.plot(xx[j],yy,'r') # use plot to draw a small segment
                        if spin_states[i+j,1] == 1:
                                if updown == 1:
                                        plt.text(xx[j,0],ksenergy+maxy/20, '%.2f'%ksenergy)
                                        ax.arrow(meanx,ksenergy-maxy/60.,0,maxy/40,head_width=maxx/40,head_length=maxy/30,color='k')
                                else:
                                        plt.text(xx[j,0],ksenergy+maxy/30, '%.2f'%ksenergy)
                                        ax.arrow(meanx,ksenergy+maxy/60.,0,-maxy/40,head_width=maxx/40,head_length=maxy/30,color='k')
                        else:
                                plt.text(xx[j,0],ksenergy+maxy/100, '%.2f'%ksenergy)
                i = i+degenerate
                first_ks = 0


def plot_ks(ax,ksupstates,ksdownstates,maxx,maxy, defectenergyVBM,tol=0.5, initial_mode=2):
        '''
        (1) If energy difference is within some tolerance, they will be considered 'degenerate' and will be plotted horizontally
        (2) If the levels are only a few, should plot them in order from left to right regardless of how far their energies are
                Practically, only plot degenerally the first few levels. After the initial plotting, this plotting method is turned off.
        Choose (1) or (2) by initial_mode. See the comments in function initialized_first_ks
        '''
        plot_panel_ks(ax, ksupstates, maxx, maxy, defectenergyVBM, tol=0.5, initial_mode=initial_mode, updown=1)
        plot_panel_ks(ax, ksdownstates, maxx, maxy, defectenergyVBM, tol=0.5, initial_mode=initial_mode, updown=0)

#
initial_mode=1 # see the comment at line 146

# energy of CBM and VBM in perfect supercells
perfenergyCBM,perfenergyVBM = cbm_vbm(f1)
# calculate VBM of defect cell by potential alignment
#mydict=read_incar(f2, incar='DEFECT')
#freycorr=float(mydict['FREYCORRALL'])
C=read_C(f2) # defect minus perfect
# calculate CBM of defect cell by averaging
bandgap = perfenergyCBM-perfenergyVBM
defectenergyVBM = C+perfenergyVBM
defectenergyCBM = defectenergyVBM + bandgap
print('In defect, VBM=%s, CBM=%s' % (defectenergyVBM, defectenergyCBM))

# get data: KS states
#ksupstates=np.array([3.2,4.7])
#ksdownstates=np.array([3.2,4.7])
ksupstates, ksdownstates, spin = read_eigenvalues(f2,defectenergyCBM,defectenergyVBM)
#ksupstates = ksupstates-defectenergyVBM
#ksdownstates = ksdownstates-defectenergyVBM


# setting
cbmregion = 2
vbmregion = 2

largestx=1
x=np.linspace(0,largestx,50)
maxy=int(bandgap+cbmregion)


# plot states
ax=plt.subplot(111)
ax.fill_between(x,-vbmregion, 0, color='skyblue') #defectenergyCBM,defectenergyCBM+cbmregion)
ax.fill_between(x,bandgap,bandgap+cbmregion,color='skyblue') #'#1f77b4') # defectenergyVBM,defectenergyVBM-vbmregion)
plt.text( largestx/15,-vbmregion+0.6,'VB' )
plt.text( largestx/15, bandgap+vbmregion-1.4,'CB' )
plot_ks(ax,ksupstates,ksdownstates,largestx,bandgap, defectenergyVBM,initial_mode=initial_mode)

# draw extra
plt.axvline(x[-1]/2, color='k',ls='--') # a vertical line to separate spin up and spin down

# title limit
ax.set_title('%s spin S=%s'%(defecttype, np.round(spin,2)))
ax.set_xlim([min(x), max(x)])
ax.set_ylim([-vbmregion, bandgap+cbmregion])
#ax.set_xlabel()
ax.set_ylabel('Energy [eV]')

# spines
ax.get_xaxis().set_visible(False) # remove x axis 
ax.spines['left'].set_linewidth(3) # thickness of the boundary
ax.spines['right'].set_linewidth(3)
ax.spines['top'].set_linewidth(3)
ax.spines['bottom'].set_linewidth(3)

# ticks
#ax.set_xticks() # list of floats
yticks = np.linspace(0, maxy,maxy+1)
ax.set_yticks(yticks)

# locators
import matplotlib.ticker as tck
#ax.yaxis.set_minor_locator(tck.AutoMinorLocator()) # have y minor locators
#plt.tick_params(axis='y', which='minor', width=1., length=3, color='k', right=True)
plt.tick_params(axis='y', width=2,length=4)  #, labelsize=15
#ax.tick_params(top=False,bottom=False,right=True,left=True) # whether the major ticks will appear

# save plot
#plt.show()
figname = '%s_ks_states.pdf' % (defecttype[1:len(defecttype)-1]).replace('\mathit','').replace('\mathrm','').replace('}','').replace('{','')
plt.tight_layout()
plt.savefig(figname,dpi=600)
print('%s is generated' % (figname))
