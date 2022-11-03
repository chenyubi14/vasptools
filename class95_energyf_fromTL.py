
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


# In[2]:
"""
Plot formation energy which come from other's calculations:
    (1) assume transition level and neutral E^f is provided
    (2) assume E^f at E_F=0 is provided for each charge state
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


class defectinfo:
    def __init__(self, defecttype, charge, energyf):
        self.defecttype = defecttype # antiO
        self.charge = charge # a list of charges [1,0,-1]
        self.energyf = energyf


class energyf_fromTL:
    def __init__(self, list_defect,bandgap, comment='',condition='',alpha=np.ones(100)):
        # defect folder information in a dictionary
        self.numberdecimal=3 # number of decimals
        #self.colors = ['black','navy','tab:red','tab:blue','tab:green','tab:orange','tab:purple','tab:brown','tab:gray','tab:pink','tab:olive']
        self.colors = ['tab:purple','navy','tab:pink','black','tab:blue','tab:brown','tab:red','tab:green','tab:orange','tab:gray','tab:olive']
        # important colors are defined in front, importance decline in order
        self.colors = list(self.colors[:len(list_defect)])
        self.colors.reverse() # use colors backward
        self.list_defect=list_defect
        self.bandgap=bandgap
        self.comment=comment
        self.condition=condition
        self.alpha=list(alpha)# set occpacity of lines, range is [0,1], 0 is transparent, 1 is solid
        self.alpha.reverse() 


    def calc_formation_energy(self, q,energyf_vbm_q, fermi_level, numdatapoints=1001):
        print('\n%se E^f(E_F=0)=%.3f' % (q, energyf_vbm_q))
        # expression of formation energy
        formation_energy_q = energyf_vbm_q + q*fermi_level
        return formation_energy_q
        
    
    def draw_lowerbound(self, defect_onetype, defectlabeltextposition=None, numdatapoints=1001, charge_seg=False,xrange=5,yrange=20):
        '''
        draw the lower bound of each data
        (1) calculate the formation energy of each charge
        (2) obtain the lower bound of the formation energies
        '''
        fermi_level = np.linspace(0,self.bandgap,numdatapoints)
        formation_energy = []
        mark_transition_index = []
        for i in range(len(defect_onetype.charge)): # for each charge of one type of defect
            q=defect_onetype.charge[i] # charges of each type of defect
            energyf_vbm_q=defect_onetype.energyf[i]
            # read the energies 
            formation_energy_q = self.calc_formation_energy(q, energyf_vbm_q, fermi_level, numdatapoints=numdatapoints) # q=charge
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
        print('transition points: x=%s y=%s' % (fermi_level[mark_transition_index],formation_energy[mark_transition_index]))
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
        # put the defect type near lines
        label_x_ind = mark_transition_index[defectlabeltextposition[0]] #mark_transition_index[int(len(mark_transition_index)/2)] #-b/2/a
        label_y_pos = formation_energy[label_x_ind] + defectlabeltextposition[2] * yrange/20 #np.poly1d([a,b,c])(label_x_pos)
        label_x_pos = fermi_level[label_x_ind] + defectlabeltextposition[1] * xrange/20
        label_angle_rotate = defectlabeltextposition[3]
        plt.text(label_x_pos,label_y_pos,'%s' % (defect_onetype.defecttype),fontsize=25,color=self.colors[0],rotation=label_angle_rotate)
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
            q=defect_onetype.charge[i] # charges of each type of defect
            energyf_vbm_q=defect_onetype.energyf[i]
            # read the energies 
            formation_energy_q = self.calc_formation_energy(q, energyf_vbm_q, fermi_level, numdatapoints=numdatapoints) # q=charge
            formation_energy.append(formation_energy_q)
            plt.plot(fermi_level, formation_energy_q, color=self.colors[0], linewidth=3, label='%s %se' % (defect_onetype.defecttype,q))#linestyle='--'
        self.colors.pop(0)

    def myplot(self,charge_seg,lower_bound_plot=True,xlimits=[0, 11.3],ylimits=[-20,25],defectlabeltextposition=None):
        fig=plt.figure()
        fig.set_size_inches(8, fig.get_figheight(), forward=True)
        fig.set_size_inches(8, fig.get_figwidth(), forward=True)
        ax=fig.add_subplot(1,1,1)
        # plot each defect
        if lower_bound_plot: # find and save transition levels
            for defect_onetype in self.list_defect:
                print('\n %s' % (defect_onetype.defecttype))
                self.draw_lowerbound(defect_onetype,charge_seg=charge_seg,defectlabeltextposition=defectlabeltextposition[defect_onetype],xrange=abs(np.diff(xlimits)),yrange=abs(np.diff(ylimits)))
        else: 
            for defect_onetype in self.list_defect:
                print('\n %s' % (defect_onetype.defecttype))
                self.draw_all_NOlowerbound(defect_onetype, charge_seg=charge_seg)
        print('\ndraw diagram...')
        plt.xlabel(r'$E_\mathrm{F}$ (eV)', fontsize=30) # Fermi energy
        plt.ylabel(r'$E^f$ (eV)', fontsize=30) # defect formation energy
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
        ax.xaxis.set_major_locator(MultipleLocator(1)) # spacing in x ticks
        ax.yaxis.set_major_locator(MultipleLocator(1)) # spacing in y ticks
        ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
        ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
        plt.tick_params(axis='y', which='minor', width=1.5, length=5, color='k', right=True)
        plt.tick_params(axis='x', which='minor', width=1.5, length=5, color='k', right=True)
        myupperleft=lambda mm,nn:mm[0]+(mm[1]-mm[0])*nn
        plt.text(myupperleft(xlimits,0.05),myupperleft(ylimits,0.9),self.condition.replace('-',' '), fontsize=28)
        #plt.legend(fontsize=20)
        plt.tight_layout()
        savename='%s_%s_formation-energy.pdf' % (self.condition,self.comment)
        print('figure %s/%s is produced'% (os.environ['PWD'],savename))
        plt.savefig(savename, dpi=600, bbox_inches='tight')

# In[3]:


