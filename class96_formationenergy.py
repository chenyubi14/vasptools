
# In[1]:

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import MultipleLocator

import sys
import os
sys.path.append(os.environ['SCRIPT'])
path = os.environ['PWD'] + '/'
import pymatgen as pmg
from pymatgen.io.vasp.outputs import BSVasprun, Vasprun
from class0_functions1 import parent_folder, read_incar
from class1_read import read_file_values

#matplotlib.rc('text', usetex = True)
#plt.rcParams.update({
#    "text.usetex": True,
#    "font.family": "serif",
#    "font.serif": ["Palatino"],
#})

# In[2]:
"""
Formation energy is defined as:

E_f=Etot-E_perfect-miu_i*n_i+q*(fermi+vbm)+f_correction
Etot is the total energy of the defect supercell
E_perfec is the total energy of the perfect supercell
n_i is the number of vacancy atom (If you have an O vacancy, then your n_i is -1)
fermi is your x axis
vbm is taken from your bulk calculation (the eigenvalue of the highest occupied state)
f_correction is your freysoldt correction
"""

def format_charge(q):
    if q==0:
        return '0' #str(q)
    elif q == 1:
        return '+'
    elif q == -1:
        return '$-$'
    elif q>1:
        return str(q)+'$+$'
    else: # q<-1
        return str(abs(q))+'$-$'


def chemical_potential(elements_name,folder_elements, deltamiu):
    '''
    Read bulk or molecule energy to compute chemical potential miu of an atom specie
    chemical potential = bulk/molecule_energy + delta miu
    return a dictionary of chemical potential for each specie atom
        element_name is a list of elements names: ['Be', 'O', 'H']
        folder_elements is a list of element folders ['enthalpyf_Be','enthalpyf_O','energyf_H']
        deltamiu is a dictionary of delta miu: {'Be':0, 'O':-6.014, 'H':-1.08}
    '''
    dict_miu = {}
    for i in range(len(elements_name)):
        ele_i = elements_name[i] # for i-th element
        ele_folder=folder_elements[i] # folder of this element
        # find the number of atoms
        ele_struc=pmg.core.Structure.from_file(ele_folder+'POSCAR')
        ele_formula=ele_struc.formula # like 'Be2'; formula is to find the number of atoms in element[i] of enthalpyf_element[i]
        #ele_formula_num=float(ele_formula[-1]) # the number of atoms is 'Be2'. It assumes the number of atoms < 10, so [-1] works
        ele_formula_num=float(ele_formula.split(ele_i)[-1]) # the number of atoms is 'Be2'.split('Be')[-1]
        # energy per atom
        read_fil=read_file_values(ele_folder) # read defect energy: read_fil.oszicar()
        miu_bulk = read_fil.oszicar() / ele_formula_num # energy per atom
        miu_i = np.round(miu_bulk + deltamiu[ele_i],6) # bulk/molecule_energy + delta_miu; round to 3 decimals
        dict_miu[ele_i] = miu_i
        print(' element %s has miu=%s' % (ele_i, miu_i))
    return dict_miu

def save_transition_points(transition_charges, transition_fermi, defecttype, transition_data_fil):
    # np.array(defect_onetype.charge)[set_charge_arg] =[ 2,  1,  0, -1, -2] gives the charges corresponds to each segment
    # fermi_level[mark_transition_index]=[ 0.,  4.59147 ,  4.803384,  9.795136, 10.12478 , 11.773   ] gives all segments
    # charge 2 is in the fermi segment [ 0.,  4.59147]; charge 1 is in the segment [4.59147 ,  4.803384]
    # save data in transition_data_fil by transition_data_fil.write()
    transition_fermi=np.round(transition_fermi,3)
    transition_data_fil.write( '\nDEFECTTYPE=%s\n' % (str(defecttype)))
    transition_data_fil.write( 'TRANSITIONCHARGE=%s\n' % (' '.join(list(transition_charges.astype(str)))))
    transition_data_fil.write('TRANSITIONFERMILEVEL=%s\n' % (' '.join(list(transition_fermi.astype(str)))))
    transition_data_fil.write('#For example: charge %s is in the segment %s. \n'% (transition_charges[0], transition_fermi[:2]))
    return 0

class defectinfo:
    def __init__(self, defecttype, charge, indiv_folder , formula):
        self.defecttype = defecttype # antiO
        self.charge = charge # a list of charges [1,0,-1]
        self.indiv_folder = indiv_folder # a list of individual folders ['wurtzite_31_antio/defect0e/', 'wurtzite_31_antio/defect1e/', 'wurtzite_31_antio/defect-1e/']
        self.formula = formula # {'Be',num_Be, 'O':num_O} 


class readformationenergy:
    def __init__(self,folder, folder_perfect, list_defect, miu_elements, comment='',folder_bandgap=None,alpha=np.ones(100),colors=None):
        if folder[-1] != '/':
            folder = folder + '/'
        self.folder=folder
        assert folder_perfect[0] != '/', 'the perfect folder needs to start from 2nd-class'
        hierdirecs = parent_folder(self.folder, hier=10)  # to find 1st-class materials folder
        assert len(hierdirecs) <= 2
        self.hierdirecs = hierdirecs
        self.hier1direc = self.hierdirecs[0]    # 1st-class folder Eg. '/home/yubi/work/berylliumO/'
        self.folder_perfect= self.hier1direc + folder_perfect # ='wurtzite_01_super_perfect/perfect_run'
        # defect folder information in a dictionary
        self.list_defect = list_defect # a list of defectinfo objects, list_defect=[defectinfo1,...]
        self.miu_elements = miu_elements # the chemical potential of each element for O rich or Be rich case, {'Be':7.9, 'O':5, 'condition':'Orich'}
        self.numberdecimal=6 # number of decimals
        self.colors = ['black','navy','tab:red','tab:blue','tab:green','tab:orange','tab:purple','tab:brown','tab:gray','tab:pink','tab:olive']
        if colors:
            self.colors=colors
        self.colors = list(self.colors[:len(list_defect)])
        self.colors.reverse()
        self.comment=comment
        self.alpha=list(alpha)# set occpacity of lines, range is [0,1], 0 is transparent, 1 is solid
        self.alpha.reverse() 
        if folder_bandgap != '':
            self.folder_bandgap = self.hier1direc + folder_bandgap
        else:
            self.folder_bandgap = self.folder_perfect

    def readperfect(self, folder_VBM=None):
        '''
        folder_VBM is the folder to find VBM. This is needed when the VBM is at Gamma point for a special point calculation.
        Need to set folder_VBM to a Gamma point calculation for a special Kpoint sampling
        '''
        if folder_VBM is None:
            folder_VBM = self.folder_perfect
        # information in perfect cells like unitcells or supercells
        # (1) read valence band maximum from folder_VBM (Gamma point sampling) because VBM is at Gamma
        # HERE!!! assumes VBM is at Gamma point and Gamma point is used as KPOINTS
        run = BSVasprun(folder_VBM+"vasprun.xml", parse_projected_eigen=True)
        bs = run.get_band_structure(folder_VBM+"KPOINTS") 
        self.vbm=np.round(bs.get_vbm()['energy'],self.numberdecimal)
        # (2) read bandgap from a unitcell calculation
        read_fil = read_file_values(self.folder_bandgap)
        self.bandgap = read_fil.bandgap() #np.round(bs.get_band_gap()['energy'],self.numberdecimal)
        # (3) read perfect cell total energy; may be different from folder_VBM, like for special Kpoint sampling
        read_fil=read_file_values(self.folder_perfect)
        self.E_perfect = read_fil.oszicar()
        print('E_perfect=%s BandGap=%s VBM=%s '%(self.E_perfect, self.bandgap, self.vbm))
        return self.E_perfect, self.bandgap, self.vbm

    def calc_formation_energy(self, q, defect_formula, folder_single, fermi_level, numdatapoints=1001):
        read_fil=read_file_values(folder_single) # read defect energy
        Etot = read_fil.oszicar()
        print('\n%se defect energy=%.3f' % (q, Etot))
        f_correction=0
        if q != 0:
            f_correction= float(read_incar(folder_single, incar='DEFECT')['FREYCORRALL']) # read freysoldt correction
            print(' charged defect freysoldt_correction=%s'% (f_correction))
        n_dot_miu = 0 # initialize n dot miu
        for key in defect_formula:
            n_dot_miu += defect_formula[key] * self.miu_elements[key] # sum n*miu for each element
        # expression of formation energy
        formation_energy_q = Etot - self.E_perfect- n_dot_miu + q*fermi_level + q*self.vbm + f_correction
        print('%.3f+fermi, q*vbs=%.3f'% (Etot - self.E_perfect- n_dot_miu + q*self.vbm + f_correction, q*self.vbm))
        return formation_energy_q
        
    
    def draw_lowerbound(self, defect_onetype, transition_data_fil,defectlabeltextposition=None, numdatapoints=1001, charge_seg=False,xrange=5,yrange=20):
        '''
        draw the lower bound of each data
        (1) calculate the formation energy of each charge
        (2) obtain the lower bound of the formation energies
        '''
        fermi_level = np.linspace(0,self.bandgap,numdatapoints)
        formation_energy = []
        mark_transition_index = []
        for i in range(len(defect_onetype.indiv_folder)): # for each charge of one type of defect
            q=defect_onetype.charge[i] # charges of each type of defect
            formula=defect_onetype.formula # a dictionary of defect formation formula like {Be:+1, O:-1}
            folder_single=defect_onetype.indiv_folder[i] # the folder storing a single defect with charge q
            # read the energies 
            formation_energy_q = self.calc_formation_energy(q, formula, folder_single, fermi_level, numdatapoints=numdatapoints) # charge, formula, folder
            formation_energy.append(formation_energy_q)
        formation_energy = np.array(formation_energy)
        formation_energy_charge_arg = list(np.argmin(formation_energy,axis=0)) # find the charge of defect that gives the lowest energy
        # e.g. formation_energy_charge_arg=[0,0,..,1,1,..,3,3,..4,4] The charge is given by defect_onetype.charge[0], [1], [3], [4]
        # want to mark the transition point between charges
        set_charge_arg = list(set(formation_energy_charge_arg)) # like [1,2,4] the set that has transitions of charges
        for i in range(0,len(set_charge_arg)): # get the index that charge transitions happens
            mark_transition_index.append(formation_energy_charge_arg.index(set_charge_arg[i]))# this index gives x=fermi_level(index),y=formation_energy(index) position of transition
        mark_transition_index.append(numdatapoints-1) # add the end point
        formation_energy = np.min(formation_energy,axis=0) # minimize over the charged defect to find the lowest energy
        #print('transition points: x=%s y=%s' % (fermi_level[mark_transition_index],formation_energy[mark_transition_index]))
        plt.scatter(fermi_level[mark_transition_index],formation_energy[mark_transition_index], color=self.colors[0]) # scatter plot transition points
        # fermi_level[mark_transition_index] is the fermi_level at transitions
        # formation_energy[mark_transition_index] is the formation energy at transitions
        if charge_seg: # plot charge labels of each segment
            for i in range(0, len(set_charge_arg)):
                left = mark_transition_index[i]
                right = mark_transition_index[i+1]
                mark_transition_index_i = int(np.average([left,right]))-1
                # the charge of this defect is defect_onetype.charge[set_charge_arg[i]]
                if defect_onetype.charge[set_charge_arg[i]]>0:
                    plt.text(fermi_level[mark_transition_index_i]-0.2, formation_energy[mark_transition_index_i]+0.3, format_charge(defect_onetype.charge[set_charge_arg[i]]), color=self.colors[0], fontsize=20)
                elif defect_onetype.charge[set_charge_arg[i]]<0:
                    plt.text(fermi_level[mark_transition_index_i]+0.2, formation_energy[mark_transition_index_i]+0.2, format_charge(defect_onetype.charge[set_charge_arg[i]]), color=self.colors[0], fontsize=20)
                else:
                    plt.text(fermi_level[mark_transition_index_i]-0.1, formation_energy[mark_transition_index_i]+0.2, format_charge(defect_onetype.charge[set_charge_arg[i]]), color=self.colors[0], fontsize=20)
        plt.plot(fermi_level, formation_energy, color=self.colors[0], linewidth=3,alpha=self.alpha[-1])#linestyle='--', label='%s' % (defect_onetype.defecttype)
        self.alpha.pop() # set transparity by alpha. Pop is to remove the last alpha used
        #[a,b,c]=np.polyfit(fermi_level, formation_energy,2) # [a,b,c] in ax^2+bx+c
        # put the defect type near lines
        label_x_ind = mark_transition_index[defectlabeltextposition[0]] #mark_transition_index[int(len(mark_transition_index)/2)] #-b/2/a
        label_y_pos = formation_energy[label_x_ind] + defectlabeltextposition[2] * yrange/20 #np.poly1d([a,b,c])(label_x_pos)
        label_x_pos = fermi_level[label_x_ind] + defectlabeltextposition[1] * xrange/20
        label_angle_rotate = defectlabeltextposition[3]
        plt.text(label_x_pos,label_y_pos,'%s' % (defect_onetype.defecttype),fontsize=25,color=self.colors[0],rotation=label_angle_rotate)
        save_transition_points(np.array(defect_onetype.charge)[set_charge_arg], fermi_level[mark_transition_index], defect_onetype.defecttype, transition_data_fil)
        # np.array(defect_onetype.charge)[set_charge_arg] =[ 2,  1,  0, -1, -2] gives the charges corresponds to each segment
        # fermi_level[mark_transition_index]=[ 0.,  4.59147 ,  4.803384,  9.795136, 10.12478 , 11.773   ] gives all segments
        # charge 2 is in the fermi segment [ 0.,  4.59147]; charge 1 is in the sement [4.59147 ,  4.803384]
        # save data in transition_data_fil by transition_data_fil.write()
        self.colors.pop(0)

    
    def draw_all_NOlowerbound(self, defect_onetype, numdatapoints=1001, charge_seg=False):
        '''
        draw the all charges of each defect data
        (1) calculate the formation energy of each charge
        (2) plot all of them without finding the lower bound of the formation energies
        '''
        fermi_level = np.linspace(0,self.bandgap,numdatapoints)
        formation_energy = []
        mark_transition_index = []
        for i in range(len(defect_onetype.indiv_folder)):
            q=defect_onetype.charge[i]
            formula=defect_onetype.formula # a dictionary of defect formation formula
            folder_single=defect_onetype.indiv_folder[i] # the folder storing a single defect with charge q
            # read the energies 
            formation_energy_q = self.calc_formation_energy(q, formula, folder_single, fermi_level, numdatapoints=numdatapoints) # charge, formula, folder
            formation_energy.append(formation_energy_q)
            plt.plot(fermi_level, formation_energy_q, color=self.colors[0], linewidth=3, label='%s %se' % (defect_onetype.defecttype,q))#linestyle='--'
        self.colors.pop(0)

    def myplot(self,charge_seg,lower_bound_plot=True,xlimits=[0, 11.3],ylimits=[-20,25],defectlabeltextposition=None):
        fig=plt.figure()
        fig.set_size_inches(6, fig.get_figheight(), forward=True)
        fig.set_size_inches(12, fig.get_figwidth(), forward=True)
        ax=fig.add_subplot(1,1,1)
        # plot each defect
        if lower_bound_plot: # find and save transition levels
            transition_data_fil = open('graphdata/%s_%s_transitionlevel.txt' % (self.miu_elements['condition'],self.comment), 'w')
            for defect_onetype in self.list_defect:
                print('\n %s' % (defect_onetype.defecttype))
                self.draw_lowerbound(defect_onetype, transition_data_fil,charge_seg=charge_seg,defectlabeltextposition=defectlabeltextposition[defect_onetype],xrange=abs(np.diff(xlimits)),yrange=abs(np.diff(ylimits)))
            transition_data_fil.close()
        else: 
            for defect_onetype in self.list_defect:
                print('\n %s' % (defect_onetype.defecttype))
                self.draw_all_NOlowerbound(defect_onetype, charge_seg=charge_seg)
        #transition_data_fil = open('graphdata/%s_%s_transitionlevel.txt' % (self.miu_elements['condition'],self.comment), 'w')
        #for defect_onetype in self.list_defect:
        #    #self.plot_onetype(dict_defect)
        #    print('\n %s' % (defect_onetype.defecttype))
        #    if lower_bound_plot:
        #        self.draw_lowerbound(defect_onetype, transition_data_fil,charge_seg=charge_seg)
        #    else:
        #        self.draw_all_NOlowerbound(defect_onetype, charge_seg=charge_seg)
        #transition_data_fil.close()
        print('\ndraw diagram...')
        plt.xlabel(r'$E_\mathrm{F}$ (eV)', fontsize=30) # Fermi energy
        plt.ylabel(r'$E^f$ (eV)', fontsize=30) # defect formation energy
        #ax.set_xlim([0, self.bandgap])
        ax.set_xlim(xlimits) 
        ax.set_ylim(ylimits)
        plt.tick_params(axis='y', width=3,length=7, labelsize=30) 
        plt.tick_params(axis='x', width=3, length=7,  labelsize=30) 
        ax.tick_params(bottom=True, top=False, left=True, right=True)

        ax.spines['left'].set_linewidth(3)
        ax.spines['right'].set_linewidth(3)
        ax.spines['top'].set_linewidth(3)
        ax.spines['bottom'].set_linewidth(3)
        #ax.figure.autofmt_xdate()
        #ax.figure.autofmt_ydate()

        #plt.xticks(np.arange(0, self.bandgap, 1))
        #plt.yticks(np.arange(ylimits[0], ylimits[1], 5))

        import matplotlib.ticker as tck
        ax.xaxis.set_major_locator(MultipleLocator(1))
        ax.yaxis.set_major_locator(MultipleLocator(5))
        ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
        ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
        plt.tick_params(axis='y', which='minor', width=1.5, length=5, color='k', right=True)
        plt.tick_params(axis='x', which='minor', width=1.5, length=5, color='k', right=True)
        myupperleft=lambda mm,nn:mm[0]+(mm[1]-mm[0])*nn
        plt.text(myupperleft(xlimits,0.05),myupperleft(ylimits,0.9),self.miu_elements['condition'].replace('-',' '), fontsize=28)
        #plt.legend(fontsize=20)
        plt.tight_layout()
        savename='graphdata/%s_%s_formation-energy.pdf' % (self.miu_elements['condition'],self.comment)
        print('figure %s/%s is produced'% (os.environ['PWD'],savename))
        plt.savefig(savename, dpi=600, bbox_inches='tight')

# In[3]:

