from glob import glob
from pathlib import Path
from pymatgen.core import Structure
from pymatgen.io.vasp.outputs import Vasprun
from nonrad.ccd import get_dQ
import matplotlib.pyplot as plt
import sys
import os
import numpy as np
from matplotlib.ticker import MultipleLocator
from typing import List, Optional, Tuple, Union
from scipy.optimize import curve_fit
from nonrad.nonrad import AMU2KG, ANGS2M, EV2J, HBAR

sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import read_incar
from class0_functions3 import write_INFO 
from class1_read import read_file_values

# draw CC diagram
# This file does not fit. I can use ground_omega and excited_omega, which are given by fitting already
# I used to fit by myself
#   #The initial fitting scheme I used: np.polyfit, need more data to be accurate
#   #Later fitting scheme: a(x-x0)^2+y0, good for cheap calculations (fewer data)

print('\n\tIf using Expanse, the excited calculations may not work! Need to rerun with Stampede\n')

SMALL_SIZE = 12
MEDIUM_SIZE = 12
BIGGER_SIZE = 16

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=MEDIUM_SIZE)  # fontsize of the figure title

pwd = os.environ['PWD'] + '/'
defect_dict=read_incar(pwd, incar='SAVEINFO')
comment=defect_dict['DEFECTTYPE'] # like DEFECTTYPE=$\mathit{V}_\mathrm{O}$
comment=comment[1:len(comment)-1] # \mathit{V}_\mathrm{O}


# (0) some functions:
def makefolderformat(folname):
    if folname[-1] != '/':
        folname=folname+'/'
    return folname


def format_charge(q):
    if q==0:
        return '0' #str(q)
    elif q == 1:
        return '+'
    elif q == -1:
        return '-'
    elif q>1:
        return str(q)+'+'
    else: # q<-1
        return str(abs(q))+'-'


def extract_charge_comment(folname, formatfolder=1):
    # get the charge between 'defect' and 'e' inside 'defect-1e'
    if formatfolder == 0:
        return int(folname.split('defect')[-1].split('e')[0]) # return only the charge number
    elif formatfolder == 1:
        return folname.split('defect')[-1].split('/')[0] # return the charge along with 'e'


def find_transition_level(larger_charge,smaller_charge,carriertype):
    transition_charges=defect_dict['TRANSITIONCHARGE'].split() #np.array(defect_dict['TRANSITIONCHARGE'].split()).astype(int)
    transitionfermilevel=defect_dict['TRANSITIONFERMILEVEL'].split() #np.array(defect_dict['TRANSITIONFERMILEVEL'].split()).astype(float)
    bandgap=transitionfermilevel[-1]
    found=0
    for i in range(len(transition_charges)-1):
        if transition_charges[i] == str(larger_charge):
            assert transition_charges[i+1] == str(smaller_charge), 'Error! The transition charges are not at transition levels'
            transitionfermilevel = transitionfermilevel[i+1]
            found=1
            break
    if found==0:
        print('Error! Not found transition charges!')
        sys.exit()
    if carriertype == 'h^+': # hole
        transitionfermilevel=float(transitionfermilevel)
    elif carriertype == 'e^-': # electron
        transitionfermilevel=float(bandgap)-float(transitionfermilevel)
    else:
        print('Error! Carriertype not recognizable')
        sys.exit()
    print(transitionfermilevel)
    return transitionfermilevel

def find_final_energy_difference(ground_folname,excited_folname):
    read_fil=read_file_values(pwd)
    E_1=read_fil.oszicar(ground_folname)
    E_2=read_fil.oszicar(excited_folname)
    dE=E_2-E_1
    return dE

def find_zpl(ground_folname,excited_folname,comment):
    ## return only the charge number with formatfolder=0
    ground_folname_charge  =extract_charge_comment(ground_folname, formatfolder=0) 
    excited_folname_charge =extract_charge_comment(excited_folname,formatfolder=0)
    if ground_folname_charge > excited_folname_charge:
        # hole capture, like Defect(0e) <-> Defect(-1e) + hole(+1e)
        carriertype='h^+'
        dE=find_transition_level(ground_folname_charge,excited_folname_charge,carriertype)
        groundlabel='$%s^{%s}$' % (comment, format_charge(ground_folname_charge) )
        excitedlabel='$%s^{%s}+%s$' % (comment, format_charge(excited_folname_charge), carriertype)
    elif ground_folname_charge < excited_folname_charge:
        # electron capture, like Defect(-1e) <-> Defect(0e) + electron(-1e)
        carriertype='e^-'
        dE=find_transition_level(excited_folname_charge,ground_folname_charge,carriertype)
        groundlabel='$%s^{%s}$' % (comment, format_charge(ground_folname_charge) )
        excitedlabel='$%s^{%s}+%s$' % (comment, format_charge(excited_folname_charge), carriertype)
    else:
        # internal transitions
        dE=find_final_energy_difference(ground_folname,excited_folname)
        carriertype=''
        defect_ground_dict=read_incar(ground_folname, incar='SAVEINFO')
        comment_ground=defect_ground_dict['COMMENT'] # COMMENT=ground triplet
        defect_excited_dict=read_incar(excited_folname, incar='SAVEINFO')
        comment_excited=defect_excited_dict['COMMENT'] # COMMENT=excited triplet
        groundlabel='$%s^{%s}$ %s' % (comment, format_charge(ground_folname_charge), comment_ground)
        excitedlabel='$%s^{%s}$ %s' % (comment, format_charge(excited_folname_charge), comment_excited)
        comment = '%s^{%s}' % (comment, format_charge(ground_folname_charge))
    return dE,carriertype,groundlabel,excitedlabel,comment

def get_omega_from_PES(
        Q: np.ndarray,
        energy: np.ndarray,
        Q0: Optional[float] = None,
        ax=None,
        q: Optional[np.ndarray] = None
) -> float:
    """Calculate the harmonic phonon frequency for the given PES.

    Parameters
    ----------
    Q : np.array(float)
        array of Q values (amu^{1/2} Angstrom) corresponding to each vasprun
    energy : np.array(float)
        array of energies (eV) corresponding to each vasprun
    Q0 : float
        fix the minimum of the parabola (default is None)
    ax : matplotlib.axes.Axes
        optional axis object to plot the resulting fit (default is None)
    q : np.array(float)
        array of Q values to evaluate the fitting function at

    Returns
    -------
    float
        harmonic phonon frequency from the PES in eV
    float
        fitted Q0
    float 
        fitted dE
    """
    def f(Q, omega, Q0, dE):
        return 0.5 * omega**2 * (Q - Q0)**2 + dE

    # set bounds to restrict Q0 to the given Q0 value
    bounds = (-np.inf, np.inf) if Q0 is None else \
        ([-np.inf, Q0 - 1e-10, -np.inf], [np.inf, Q0, np.inf])
    popt, _ = curve_fit(f, Q, energy, bounds=bounds)    # pylint: disable=W0632

    # optional plotting to check fit
    if ax is not None:
        q_L = np.max(Q) - np.min(Q)
        if q is None:
            q = np.linspace(np.min(Q) - 0.1 * q_L, np.max(Q) + 0.1 * q_L, 1000)
        ax.plot(q, f(q, *popt))

    return HBAR * popt[0] * np.sqrt(EV2J / (ANGS2M**2 * AMU2KG)), popt[1], popt[2]

# (1) initialize folder names (folname)
if len(sys.argv) < 3:
    print('Error! Which charge defects do you want to use? Enter ground_folder, excited_folder')
    print('Can also enter nonrad_displacements.charge.json about E_abs, E_emi, E_ZPL, groundlabel, excitedlabel, value comment')
    print('The content in json file is {"dxabs":0,"dyabs":0,"dxem":0,"dyem":0,"dxzpl":0,"dyzpl":0,"dxgr":0,"dygr":0,"dxex":0,"dyex":0,"dxval":0,"dyval":0,"dxQ":0,"dyQ":0,"rot":0}')
    #print(' E_abs, E_emi, E_ZPL, ground label, excited label, written values comment, Delta Q')
    sys.exit()
ground_folname = pwd+makefolderformat(sys.argv[1]) #'defect0e'
excited_folname = pwd+makefolderformat(sys.argv[2]) #'defect-1e'
ground_folname_charge = extract_charge_comment(ground_folname)
excited_folname_charge = extract_charge_comment(excited_folname)
cc_folname = 'cc_%s_%s' % (ground_folname_charge, excited_folname_charge)
cc_dir = Path(cc_folname)
if not os.path.isdir(cc_dir): # check  folder's existence
    print('The %s folder does not exist. Enter the right arguments.' % (cc_folname))
    sys.exit()

# (2) read ZPL either from the transition energy levels (for electron or hole captures), or from final energies (for internal transitions)
dE,carriertype,groundlabel,excitedlabel,comment=find_zpl(ground_folname,excited_folname,comment)

# (3) read equilibrium structures from first-principles calculations
ground_files = Path(ground_folname)
ground_struct = Structure.from_file(str(ground_files / 'CONTCAR'))
excited_files = Path(excited_folname)
excited_struct = Structure.from_file(str(excited_files / 'CONTCAR'))

# calculate dQ
dQ = get_dQ(ground_struct, excited_struct) # amu^{1/2} Angstrom

# this prepares a list of all vasprun.xml's from the CCD calculations
ground_vaspruns = glob(str(cc_dir / 'ground' / '*' / 'vasprun.xml'))
excited_vaspruns = glob(str(cc_dir / 'excited' / '*' / 'vasprun.xml'))

# remember that the 0% displacement was removed before? we need to add that back in here
ground_vaspruns = ground_vaspruns + [str(ground_files / 'vasprun.xml')]
excited_vaspruns = excited_vaspruns + [str(excited_files / 'vasprun.xml')]

# extract the potential energy surface
def get_Q_E_from_vaspruns(vasprun_paths,dQ,Lexcited): #ground_struct, 
    # my function to read Qs and Es from vasprun.xml
    assert Lexcited==0 or Lexcited==1 # either ground or excited state
    num=len(vasprun_paths)
    Q, energy=(np.zeros(num),np.zeros(num))
    Q_code = np.zeros(num)
    for i, vr_fname in enumerate(vasprun_paths):
        vf_foldername=vr_fname[:-11] #'cc_-1e_-1e.constrainedInternal3/excited/0/vasprun.xml'[:-11]
        # remove 'vasprun.xml' and keep the folder name
        dictionary=read_incar(vf_foldername,incar='SAVEINFO')
        displacement_i = float(dictionary['DISPLACEMENT'])
        Q[i]=dQ*(displacement_i+Lexcited) # I use my own data to get dQ because Mark's code had some error
        vr=Vasprun(vr_fname,parse_dos=False,parse_eigen=False)
        Q_code[i] = get_dQ(ground_struct,vr.structures[-1]) # use Mark's code to get dQ
        energy[i]=vr.final_energy
        if displacement_i == 0:
            reference_energy = energy[i] # use the relaxed ground state or excited state as the reference
    #print('My dQ=%s\n Mark code dQ=%s' % (np.round(Q,4), np.round(Q_code,4)))
    return Q, (energy-reference_energy) # I used the minimum energy of a parabola as a reference


Q_ground, E_ground = get_Q_E_from_vaspruns(ground_vaspruns,dQ,Lexcited=0) 
Q_excited, E_excited = get_Q_E_from_vaspruns(excited_vaspruns,dQ,Lexcited=1) 
sortedind=np.argsort(Q_ground)
print('Ground dQ=%s dE=%s'%(np.round(Q_ground[sortedind],4),np.round(E_ground[sortedind],4)))
sortedind=np.argsort(Q_excited)
print('Excited dQ=%s dE=%s'%(np.round(Q_excited[sortedind],4),np.round(E_excited[sortedind],4) ))

# the energy surfaces are referenced to the minimums, so we need to add dE (defined before) to E_excited
E_excited = dE + E_excited

#fig, ax = plt.subplots(figsize=(5, 4))
fig, ax = plt.subplots(figsize=(5.5, 4))
# by passing the axis object, it also plots the fitted curve
q = np.linspace(-1.0, 3, 100)
ground_omega,ground_Q0,ground_dE = get_omega_from_PES(Q_ground, E_ground, ax=ax, q=q) # passing ax plots lines, the fitted curve
excited_omega,excited_Q0,excited_dE = get_omega_from_PES(Q_excited, E_excited, ax=ax, q=q) #plots the fitted curve
print('Ground: Omega=%.6f, Q0=%.6f (should be 0), dE=%.6f (should be 0)' % (ground_omega,ground_Q0,ground_dE) )
print('Excited: Omega=%.6f, Q0=%.6f (should be %.6f), dE=%.6f (should be %.6f)' % (excited_omega,excited_Q0, dQ,excited_dE, dE) )

# (4) data analysis: ZPL, absorption, emission
# use omega to reconstruct the fitting function
fitfunc=lambda q,q0,omega,energy0: 0.5*(omega/HBAR/np.sqrt(EV2J/(ANGS2M**2 * AMU2KG)))**2*(q-q0)**2+energy0 #
# omega is in units of eV, should convert it to the coefficient in formula omega^2 (Q-Q0)^2/2 + dE: omega/HBAR/np.sqrt(EV2J/(ANGS2M**2 * AMU2KG))
excited_fit_func=lambda q:fitfunc(q,excited_Q0,excited_omega,excited_dE) # omega^2 (Q-Q0)^2/2 + dE
ground_fit_func=lambda q: fitfunc(q,ground_Q0,ground_omega,ground_dE) # omega^2 Q^2/2

# 
absorption=excited_fit_func(0)-ground_fit_func(0)
emission=excited_fit_func(dQ)-ground_fit_func(dQ)
zpl=round(dE,3) # zero phonon line is the difference of ground minimum and excited minimum
fc_relaxation = dE-emission # Frank-Condon relaxation energy into phonons
hr_factor=fc_relaxation / ground_omega # Huang-Rhys factor # number of phonons
# find crossing points for non-radiative recombnination energy
excited_coeffcients=np.array([1,-2*dQ,dQ**2])*(excited_omega/HBAR/np.sqrt(EV2J/(ANGS2M**2 * AMU2KG)))**2*0.5+np.array([0,0,dE])
ground_coeffcients=np.array([1,0,0])*(ground_omega/HBAR/np.sqrt(EV2J/(ANGS2M**2 * AMU2KG)))**2*0.5
diff_fit_ground_excited = np.poly1d(excited_coeffcients-ground_coeffcients)
Q_cross=diff_fit_ground_excited.r # root of result being zero
NRR_energy = excited_fit_func(Q_cross) -dE # non-radiative recombination energetic energy
# currently NRR_energy is not very accurate, not seems to reproduce the curve_fit result, differ by about 0.5eV
dicti={'ground_omega':round(ground_omega,3), 
    'excited_omega': round(excited_omega,3), 
    'E_abs':round(absorption,3), 
    'E_em':round(emission,3), 
    'ZPL':zpl, 
    'E_FC':round(fc_relaxation,3), 
    'HR_factor':round(hr_factor,3), 
    'non_radiative_energetic':np.round(NRR_energy,3)}
# two decimals for energies



################################################
write_values=0 # write_values>0 for writing comments
# write_values=1 for writing values (E_abs,E_ZPL,E_em) as a comment
#   # need val
# write_values=2 for also writing values (E_FC and HR factor) as a second line
#   # need val. Same as write_values=1
# write_values=3 for further writing ground and excited labels
#   # need val, gr, ex
# write_values=4 for further writing title "defect cc diagram"
#   # need val, gr, ex. Same as write_values=3
label_mode=1
# label_mode=0 only have arrows for E_abs, E_ZPL, E_em # use mode1.json
#   # need abs, em, zpl. Use mode1
# label_mode=1 have arrows for also delta_Q and E_FC # use mode1.json
#   # need abs, em, zpl, Q, # this is mode1
#   # might use non-zero write_values, better add val, gr, ex
# label_mode=2 arrows + "values" for E_abs, E_ZPL, E_em, remove scattered data points # use mode2.json
#   # need abs, em, zpl, # this is mode2 
#   # should be more crowded due to the values ## could keep Q, might have another mode#
#   # never use write_values, don't add val, gr, ex
# label_mode=3 only have arrows for E_abs, E_ZPL, E_em, remove scattered data points # use mode1.json
#   # need abs, em, zpl. Use mode1
# label_mode=4 tilt words in mode2  # use mode4.json
#   # need abs, em, zpl, rot # could keep Q, might have another mode
#   # never use write_values, don't add val, gr, ex
################################################



# (5) plot scattered data points
#colors=['blue','red'] #,c=colors[0] , c=colors[1]
if label_mode not in (2,3,4):
    ax.scatter(Q_ground, E_ground, s=10, label=groundlabel) 
    ax.scatter(Q_excited, E_excited, s=10, label=excitedlabel) 
# ax.legend(loc='lower right')
ax.set_xlabel('$Q$ [amu$^{1/2}$ $\AA$]',fontsize=16)
ax.set_ylabel('Energy [eV]',fontsize=16)


## (6) set comments and arrows
dx1,dy1,dx2,dy2,dx3,dy3,dx4,dy4,dx5,dy5,dx6,dy6,dx7,dy7,rot=np.zeros(15)
# "dxabs":0,"dyabs":0,"dxem":0,"dyem":0,"dxzpl":0,"dyzpl":0,"dxgr":0,"dygr":0,"dxex":0,"dyex":0,"dxval":0,"dyval":0,"dxQ":0,"dyQ":0,"rot":0
print('dQ=%s'%dQ)
if len(sys.argv)>3:
    print('Read graph test displacements from %s' % (sys.argv[3]))
    import json
    filename=open(sys.argv[3],'r')
    dxdy=json.load(filename)
    print(dxdy)
    dx1=dxdy['dxabs'];dy1=dxdy['dyabs']
    dx2=dxdy['dxem'];dy2=dxdy['dyem']
    dx3=dxdy['dxzpl'];dy3=dxdy['dyzpl']



if write_values>0:
    ylimits=[-absorption*0.1,absorption*1.5]
    dx6=dxdy['dxval'];dy6=dxdy['dyval']
else:
    #ylimits=[-absorption*0.1,absorption*1.2]
    ylimits=[-absorption*0.1,absorption*1.1]
xlimits=[-0.7*dQ,1.7*dQ]
#print(ylimits)

## a. set arrows ## maybe comment the following
if label_mode>=0: #label_mode==0 or label_mode==1 or label_mode == 2:
    ax.annotate("",xy=(0,0),xytext=(0,absorption), arrowprops=dict(arrowstyle="<|-",color='black')) # absorption arrow
    ax.annotate("",xy=(dQ,dE),xytext=(dQ,dE-emission), arrowprops=dict(arrowstyle="<|-",color='black')) # emission arrow
    ax.annotate("",xy=(0,0),xytext=(dQ,excited_fit_func(dQ)), arrowprops=dict(arrowstyle="<|-|>",color='black')) # ZPL arrow
#plt.plot(np.linspace(-dQ*0.5,dQ*1.5,100),np.linspace(0,0,100),linestyle=':',color='black') # dashed line for indicating FC range
if label_mode==1:
    ax.annotate("",xy=(dQ,dE-emission),xytext=(dQ,0), arrowprops=dict(arrowstyle="<|-",color='black')) # FC arrow
    ax.annotate("",xy=(0,0),xytext=(dQ,0), arrowprops=dict(arrowstyle="<->",color='black',ls='-')) # Delta Q arrow
    plt.plot(np.linspace(0,0,100),np.linspace(0,ylimits[0],100),linestyle=':',color='black') # dashed line for indicating Q
    plt.plot(np.linspace(dQ,dQ,100),np.linspace(0,ylimits[0],100),linestyle=':',color='black') # dashed line for indicating Q_e
    dx7=dxdy['dxQ'];dy7=dxdy['dyQ']

## b. set comments 
if label_mode in (0,1,3): # add E_abs E_em E_zpl labels without values
    plt.text(0-dQ*0.5+dx1,ground_fit_func(0)+absorption/2+dy1,'$E_\mathrm{abs}$') #=%seV'%absorption)
    plt.text(dQ*1.05+dx2,excited_fit_func(dQ)-emission/2+dy2,'$E_\mathrm{em}$') # =%seV'%emission)
    plt.text(dQ*0.6+dx3, excited_fit_func(dQ)*0.5+dy3, '$E_\mathrm{ZPL}$')
if label_mode==1: # add E_FC and Delta Q labels
    plt.text(dQ*1.05+dx2,(excited_fit_func(dQ)-emission)/2,'$E_\mathrm{FC}$') # =%seV'%emission)
    plt.text(dQ/2+dx7,0+dy7,'$\Delta$Q')
if label_mode == 2: #
    plt.text(0-dQ*0.5+dx1,ground_fit_func(0)+absorption/2+dy1,'$E_\mathrm{abs}$=%seV'%(round(absorption,2)),fontsize=12) #=%seV'%absorption)
    plt.text(dQ*1.05+dx2,excited_fit_func(dQ)-emission/2+dy2,'$E_\mathrm{em}$=%seV'%(round(emission,2)),fontsize=12) # =%seV'%emission)
    plt.text(dQ*0.6+dx3, excited_fit_func(dQ)*0.5+dy3, '$E_\mathrm{ZPL}$=%seV'%(round(dE,2)),fontsize=12)
if label_mode == 4:
    plt.text(0-dQ*0.5+dx1,ground_fit_func(0)+absorption/2+dy1,'$E_\mathrm{abs}$=%seV'%(round(absorption,2)),fontsize=12,rotation=90) #=%seV'%absorption)
    plt.text(dQ*1.05+dx2,excited_fit_func(dQ)-emission/2+dy2,'$E_\mathrm{em}$=%seV'%(round(emission,2)),fontsize=12,rotation=90) # =%seV'%emission)
    rot=dxdy['rot']
    plt.text(dQ*0.6+dx3, excited_fit_func(dQ)*0.5+dy3, '$E_\mathrm{ZPL}$=%seV'%(round(dE,2)),fontsize=12,rotation=rot+np.arctan(dE/dQ* (xlimits[1]-xlimits[0]) / (ylimits[1]-ylimits[0]))/np.pi*180)

## c. write values ## 
if write_values>0:
    comment_text='$E_{\mathrm{ZPL}}$=%seV,$E_{\mathrm{abs}}$=%seV,$E_{\mathrm{em}}$=%seV' %  (zpl,round(absorption,2),round(emission,2))
    if write_values == 2:
        comment_text=comment_text+'\n\t$E_{\mathrm{FC}}$=%seV, S=%s' %  (round(fc_relaxation,2),round(hr_factor,2))
    plt.text(-0.5*dQ+dx6,absorption*1.3+dy6,comment_text,fontsize=12)
    if write_values==3:
        #plt.text(max(Q_ground),min(E_ground),groundlabel)
        dx4=dxdy['dxgr'];dy4=dxdy['dygr']
        dx5=dxdy['dxex'];dy5=dxdy['dyex']
        plt.text(dQ+dx4,min(E_ground)+dy4,groundlabel, fontsize=12)
        plt.text(min(Q_excited)+dx5,max(E_excited)+dy5,excitedlabel, fontsize=12)
    if write_values>=4:
        ax.set_title('$%s$ CC diagram'%(comment), fontsize=16)


# (7) set limits
plt.xlim(xlimits[0],xlimits[1])
plt.ylim(ylimits[0],ylimits[1])


# (8) set spines
import matplotlib.ticker as tck
ax.spines['left'].set_linewidth(1.5)
ax.spines['right'].set_linewidth(1.5)
ax.spines['top'].set_linewidth(1.5)
ax.spines['bottom'].set_linewidth(1.5)
plt.tick_params(axis='x', width=2, length=7,  labelsize=15) 
plt.tick_params(axis='y', width=2,length=7, labelsize=15) 
ax.tick_params(bottom=True, top=False, left=True, right=True)
#plt.xticks(np.linspace(xlimits[0],xlimits[1], 1))
#plt.yticks(np.lin(-20, 25, 5))
import matplotlib.ticker as tck
ax.xaxis.set_major_locator(MultipleLocator(0.5))
ax.yaxis.set_minor_locator(MultipleLocator(5))
ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
plt.tick_params(axis='y', which='minor', width=1.5, length=5, color='k', right=True)
plt.tick_params(axis='x', which='minor', width=1.5, length=5, color='k', right=True)

figname=cc_folname+'.pdf' #comme nt+
# output
print('\n\n',dicti)
#print('parabola like harmonic oscillator: ground_omega_frequency=%s, excited_omega_frequency=%s' % (round(ground_omega,3), round(excited_omega,3)))
#print('transitions: E_absorption=%seV, E_emission=%seV, ZPL_dE=%seV, ZPL_fitted_line=%seV, FC=ZPL-E_emission=%seV, Huang-Rhys_factor=%s, non-radiative_recombination_energetic=%s' % (absorption, emission, dE, zpl, fc_relaxation, hr_factor, NRR_energy))
print('NRR may have two or zero crossing points. Pick the energy that is more plausible.')
print('figure saved to %s' % figname)
write_INFO(dictionary=dicti,incarname=cc_folname+'.txt')

# save figure
plt.tight_layout()
plt.savefig(figname,dpi=600)
