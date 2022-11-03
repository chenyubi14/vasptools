import sys
import os
import numpy as np
import matplotlib.pyplot as plt
#from pymatgen import Structure
sys.path.append(os.environ['SCRIPT'])
path = os.environ['PWD'] + '/'

from class0_functions1 import find_files, savedata, read_incar
from class1_read import read_file_values
from class0_functions3 import reorder_x_y_data


class drawmulinfo:
    def __init__(self, x_str, y_str, header, middle='', lattice_structure=None, xlim=None, ylim=None, materialcomment=''):
        # fixed info for all... the format of infodict(information dictionary) is 'keyword': ('xlabel', 'unit')
        self.xfile_infodict = {'ENCUT': 'INCAR', 'AEXX': 'INCAR', 'HFSCREEN': 'INCAR','NELECT': 'INCAR', 'SCALING':'CONTCAR','bond_length':'POSCAR'}
        self.xlabel_infodict = {'ENCUT':("cutoff energy", 'eV'), 'energy':('energy', 'eV'), 'AEXX':('AEXX',''), 'HFSCREEN': ('HFSCREEN', ''), 'NELECT':('number of elections',''), 'SCALING':('volume',r'$\AA^3$'),'bond_length':('bond length',r'$\AA$')}
        self.ylabel_infodict = {'equil_energy':('final energy', 'eV'),'bandgap':('bandgap', 'eV'),'wurtzite_u':('wurtzite u parameter',''), 'lattice_para':('lattice parameter',''), 'koopmans':('$E(N-1)-E(N)+E_{corr}+\epsilon_{ho}(N)$', 'eV'), 'hoenergy':('highest occ energy','eV'), 'bond_length':('bond length',r'$\AA$')}
        # plot info provided
        self.lattice_structure = lattice_structure
        self.scalingvolume = True # use volume for scaling or use lattice parameter for scaling
        self.header = header
        self.middle=middle
        self.x_str = x_str
        self.y_str = y_str
        # plot info generated
        self.x_file_open = self.xfile_infodict[x_str]
        self.xlabel = '%s (%s)' % self.xlabel_infodict[x_str]
        self.ylabel = '%s (%s)' % self.ylabel_infodict[y_str]
        self.title = '%s: %s vs %s' % (materialcomment, self.ylabel_infodict[y_str][0], self.xlabel_infodict[x_str][0])
        self.xlim=xlim
        self.ylim=ylim
        # plot data initial
        self.xx=np.array([])
        self.data=[] #np.array([])
        # read data
        self.read()
        if not os.path.isdir(os.environ['PWD']+'/savedDATA'):
            os.system('mkdir savedDATA')
        savedata(os.environ['PWD']+'/', self.xx, self.data, self.x_str, self.y_str, self.header)
    
    def read(self):
        '''
        read variables in each subfolder:
        1- go to subfolder
        2- read x
        3- read y 
        4- extra data manipulations
        '''
        exist_folders = find_files('.', self.header, self.x_str,middle=self.middle, remove=False)
        print('folders:',exist_folders)
        for di in exist_folders:
            # 1- go to the subfolder
            subfolder=path+di
            os.chdir(subfolder) 
            print('pwd:',os.getcwd())
            read_var = read_file_values(subfolder)
            # 2- read x
            if self.x_file_open == 'INCAR':
                x=read_var.incar([self.x_str], float)[0] # format of a list
            elif self.x_file_open == 'POSCAR' or self.x_file_open == 'CONTCAR':
                if self.x_str == 'SCALING':
                    if self.scalingvolume:
                        x=read_var.poscar('volume')
                    else:
                        x=read_var.poscar('abc')
                        self.xlabel='%s (%s)'%('box length',r'$\AA$')
                elif self.x_str == 'bond_length':
                    x=read_var.poscar('bond_length')
                else:
                    print('Error! The variable to read for POSCAR is not prepared')
                    sys.exit()
            else:
                print('Error! The x file you want to read is not prepared')
                sys.exit()
            self.xx=np.append(self.xx, x) # add x to self.xx
            # 3- read y
            if self.y_str=='equil_energy':
                y = read_var.oszicar()
            elif self.y_str == 'bandgap':
                y = read_var.bandgap()
            elif self.y_str == 'wurtzite_u':
                y,_ = read_var.get_u()
            elif self.y_str == 'lattice_para':
                y, self.lattice_labels = read_var.lattice_para(self.lattice_structure)
            elif self.y_str == 'koopmans':
                # E(N-1)-E(N)+Eho+Ecorr
                # read each item for a single folder. Not getting lists, but single numbers
                positive_folder=subfolder.replace('0e_','1e_')
                z1 = read_var.oszicar(folder=positive_folder) # E(N-1)
                z2 = read_var.oszicar() # E(N) 
                z3 = read_var.eigenval_ho_lo()[0] # read Eho in neutral state;  return (energy, band)
                z4 = float(read_incar(positive_folder, incar='FREYCORR')['FREYCORR']) # Ecorr freysoldt correction
                y = z1-z2+z3+z4 # not a list
            elif self.y_str == 'hoenergy':
                y = read_var.read_single_eigenE(eigenvaltype='ho',spinpolarized=False)
            elif self.y_str == 'bond_length':
                y = read_var.poscar('bond_length')
            else:
                print('Error! The y file you want to read is not prepared')
            #self.data=np.append(self.data, y)
            self.data.append(y)
        self.data=np.array(self.data)
        os.chdir(path)
        # 4- extra data manipulations
        if self.y_str == 'lattice_para':
            self.data =np.transpose(self.data) # transpose to make data.shape = (3, N), so data[0] gives lattice parameter a(x_str)
        elif self.y_str == 'equil_energy': # energy needs to reset reference
            self.data = self.data-self.data[-1]
        elif self.x_str == 'AEXX' and self.y_str == 'bandgap': # print fitting AEXX to bandgap
            bandgapBeOvalue=11.3
            aexxvalue_BeOexpr = np.interp(bandgapBeOvalue, self.data, self.xx)
            print('AEXX=%f corresponds to experimental bandgap=%feV' % (aexxvalue_BeOexpr, bandgapBeOvalue) )
            with open( path+'savedDATA/%s_AEXX_bandgap=%.2feV' % (self.header,bandgapBeOvalue), 'w') as f:
                f.write( 'AEXX=%f  # BeO bandgap=%feV\n' % (aexxvalue_BeOexpr, bandgapBeOvalue) )
            #with open( path+'%s_AEXX_bandgap=%.2feV' % (self.header,bandgapBeOvalue), 'w') as f:
            #    f.write( 'AEXX=%f  # BeO bandgap=%feV\n' % (aexxvalue_BeOexpr, bandgapBeOvalue) )
        # print data
        self.xx=np.round(self.xx,6)
        self.data=np.round(self.data,6)
        print('xx='+', '.join(self.xx.astype(str)))
        if len(self.data.shape) > 1:
            for i in range(self.data.shape[0]):
                print('yy='+', '.join(self.data[i].astype(str)))
        else:
            print('yy='+', '.join(self.data.astype(str)))

    def plot(self):
        plt.figure()
        SMALL_SIZE = 12
        MEDIUM_SIZE = 14
        BIGGER_SIZE = 16
        plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
        plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
        plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
        plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
        plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
        plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
        plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
        # plot or scatter
        if self.y_str == 'lattice_para':
            #plt.subplots(nrows=len(data), ncols=1)
            num_plots = len(self.data)
            for i in range(num_plots):
                #sub_num = num_plots * 100 + 10 + i
                plt.subplot(num_plots, 1, i+1)
                plt.plot(self.xx, self.data[i],'-')
                if i == 0:
                    plt.title('%s' % (self.title))
                plt.ylabel(self.lattice_labels[i])
        elif self.y_str == 'koopmans':
            # Plot reference
            plt.plot(self.xx,np.zeros(len(self.xx)), label='$E(N-1)-E(N)+\epsilon_{ho}+E_{corr}=0$')
            # fit koopmans=0
            if self.x_str == 'HFSCREEN':
                numberofdata=5
                fity1=self.data[:numberofdata]
                fitx1=self.xx[:numberofdata]
            else:
                fity1=self.data
                fitx1=self.xx
            print('Fit this array to 0',fity1)
            poly=np.polyfit(fity1,fitx1,1)# fitting function
            po=np.poly1d(poly) # poly function
            var_fit=np.round(po(0),3)
            print('Fit %s=%s'%(self.x_str,var_fit))
            # change title to contain the fixed initial parameters
            relax_fix_var=['HFSCREEN', 'AEXX']
            relax_fix_var.remove(self.x_str)
            relax_fix_var = relax_fix_var[0]
            read_fil=read_file_values(path)
            relax_fix_para= self.header[-4:]#read_fil.incar([relax_fix_var])
            self.title = '$\mathit{V}_\mathrm{O}$ %s=%s fit=%s' % ( relax_fix_var, relax_fix_para, var_fit)
            # plot data
            legend='$E(N-1)-E(N)+\epsilon_{ho}+E_{corr}$'
            plt.plot(self.xx, self.data, 'o', label=legend)
            plt.legend()
        else:
            plt.plot(self.xx, self.data,'o')
        # plot setting
        plt.xlabel(self.xlabel) # x label 
        if self.xlim:plt.xlim(self.xlim) # set limits
        if self.ylim:plt.ylim(self.ylim)
        if self.y_str=='lattice_para':
            pass # done previously
        else:
            plt.title(self.title) # add title
            plt.ylabel(self.ylabel) # y label, from y_str
        
        # save
        plt.tight_layout()
        figname="%s-%s_%s.pdf" % (self.header,self.y_str, self.x_str)
        print('save figure:%s/%s'% (os.environ['PWD'],figname))
        plt.savefig(path+figname,dpi=600)
        plt.close()



class drawmulinfo3D:
    def __init__(self, x_str, y_str, z_str, header, middle='', lattice_structure=None, xlim=None, ylim=None, zlim=None):
        # fixed info for all... the format of infodict(information dictionary) is 'keyword': ('xlabel', 'unit')
        self.xfile_infodict = {'ENCUT': 'INCAR', 'AEXX': 'INCAR', 'HFSCREEN': 'INCAR'}
        self.yfile_infodict = self.xfile_infodict
        self.xlabel_infodict = {'ENCUT':("Cutoff energy", 'eV'), 'energy':('energy', 'eV'), 'AEXX':('AEXX',''), 'HFSCREEN':('HSCREEN', '')}
        self.ylabel_infodict = self.xlabel_infodict
        self.zlabel_infodict = {'equil_energy':('equil_energy', 'eV'),'bandgap':('bandgap', 'eV'),'wurtzite_u':('wurtzite u parameter',''), 'koopmans':('$E(N-1)-E(N)+E_{corr}+\epsilon_{ho}(N)$', 'eV'), 'eps':('electronic dielectric constant','')}
        # 'lattice_para':('lattice parameter','')
        # plot info provided
        self.lattice_structure = lattice_structure
        self.header = header
        self.middle = middle
        self.x_str = x_str
        self.y_str = y_str
        self.z_str = z_str
        # plot info generated
        self.x_file_open = self.xfile_infodict[x_str]
        self.y_file_open = self.yfile_infodict[y_str]
        self.xlabel = '%s %s' % self.xlabel_infodict[x_str]
        self.ylabel = '%s %s' % self.ylabel_infodict[y_str]
        self.zlabel = '%s %s' % self.zlabel_infodict[z_str]
        self.title = '%s(%s,%s)' % (z_str, x_str, y_str)
        self.xlim=xlim
        self.ylim=ylim
        self.zlim=zlim
        # plot data initial
        self.xx=np.array([])
        self.yy=np.array([])
        self.data=[]#np.array([])
        # read data
        self.read() # arguments are already saved self.arguments
        savedata(os.environ['PWD']+'/', np.stack((self.xx, self.yy)), self.data, self.x_str+'_'+self.y_str, self.z_str, self.header)
    
    def read(self):
        '''
        read variables in each subfolder:
        1- go to subfolder
        2- read x
        3- read y 
        4- read data
        5- extra data manipulations
        '''
        exist_folders = find_files('.', self.header, self.x_str, middle=self.middle, remove=False)
        print('folders:',exist_folders)
        for di in exist_folders:
            # 1- go to the subfolder
            subfolder=path+di
            os.chdir(subfolder) 
            print('pwd:',os.getcwd())
            read_var = read_file_values(subfolder)
            # 2- read x
            if self.x_file_open == 'INCAR':
                x,y=read_var.incar([self.x_str,self.y_str], float) # format of a list
            else:
                print('Error! The x file you want to read is not prepared')
            self.xx=np.append(self.xx, np.round(x,4)) # add x to self.xx
            self.yy=np.append(self.yy, np.round(y,4)) # add y to self.yy
            # 3- read y
            if self.z_str=='equil_energy':
                z = read_var.oszicar()
            elif self.z_str == 'bandgap':
                z = read_var.bandgap()
            elif self.z_str == 'eps':
                z = read_var.outcar_dielec_const()
            elif self.z_str == 'wurtzite_u':
                z,_ = read_var.get_u()
            elif self.z_str == 'koopmans':
                # E(N-1)-E(N)+Eho+Ecorr
                # read each item for a single folder. Not getting lists, but single numbers
                positive_folder=path+di.replace('0e_','1e_')
                z1 = read_var.oszicar(folder=positive_folder) # E(N-1)
                z2 = read_var.oszicar() # E(N) 
                z3 = read_var.eigenval_ho_lo()[0] # read Eho in neutral state;  return (energy, band)
                z4 = float(read_incar(positive_folder, incar='DEFECT')['FREYCORRELEC']) # Ecorr freysoldt correction
                z = z1-z2+z3+z4 # not a list
            elif self.z_str == 'lattice_para':
                z, self.lattice_labels = read_var.lattice_para(self.lattice_structure)
            else:
                print('Error! The z file you want to read is not prepared')
            #self.data=np.append(self.data, y)
            self.data.append(np.round(z,4)) # self.data is a list
        # reshape the data format
        # the data should be meshgrid
        #orderxx=np.unique(self.xx)
        #orderyy=np.unique(self.yy)
        #self.data=np.reshape(self.data,(len(self.yy), len(self.xx))) # data changes with x first, so shape=(yy,xx)
        ## put the change along x in the last axis
        #self.xx,self.yy=np.meshgrid(self.xx,self.yy) # meshgrid makes x change along the last axis, so is consistent with data
        self.xx,self.yy,self.data=reorder_x_y_data(self.xx,self.yy,self.data) # return ordered meshgrid data
        os.chdir(path)
        # 4- extra data manipulations
        if self.z_str == 'lattice_para':
            #self.data =np.transpose(self.data) # transpose to make data.shape = (3, N), so data[0] gives lattice parameter a(x_str)
            print('The current data format is not working for multi data variables')
        elif self.z_str == 'equil_energy': # energy needs to reset reference
            self.data = self.data-self.data[-1]
        #elif (self.x_str == 'AEXX' or ) and self.z_str == 'bandgap':
        #    bandgapBeOvalue=11.3
        #    #aexxvalue_BeOexpr = np.interp(bandgapBeOvalue, self.data, self.xx)
        #    #print('AEXX=%f corresponds to experimental bandgap=%feV' % (aexxvalue_BeOexpr, bandgapBeOvalue) )
        #    #with open( path+'savedDATA/%s_AEXX_bandgap=%.2feV' % (self.header,bandgapBeOvalue), 'w') as f:
        #    #    f.write( 'AEXX=%f  # BeO bandgap=%feV\n' % (aexxvalue_BeOexpr, bandgapBeOvalue) )
        #    #with open( path+'%s_AEXX_bandgap=%.2feV' % (self.header,bandgapBeOvalue), 'w') as f:
        #    #    f.write( 'AEXX=%f  # BeO bandgap=%feV\n' % (aexxvalue_BeOexpr, bandgapBeOvalue) )
        # print data
        #self.xx=np.round(self.xx,6)
        #self.yy=np.round(self.yy,6)
        #self.data=np.round(self.data,6)
        # print data
        print('xx='+', '.join(np.unique(self.xx).astype(str)))
        print('yy='+', '.join(np.unique(self.yy).astype(str)))
        if len(self.data.shape) > 1:
            for i in range(self.data.shape[0]):
                print('zz='+', '.join(self.data[i].astype(str)))
        else:
            print('zz='+', '.join(self.data.astype(str)))

    def plot(self):
        plt.figure()
        SMALL_SIZE = 12
        MEDIUM_SIZE = 14
        BIGGER_SIZE = 16
        plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
        plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
        plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
        plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
        plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
        plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
        plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
        # plot or scatter
        if self.z_str == 'lattice_para':
            #plt.subplots(nrows=len(data), ncols=1)
            num_plots = len(self.data)
            for i in range(num_plots):
                #sub_num = num_plots * 100 + 10 + i
                plt.subplot(num_plots, 1, i+1)
                plt.plot(self.xx, self.data[i],'-')
                if i == 0:
                    plt.title('%s' % (self.title))
                plt.ylabel(self.lattice_labels[i])
        else:
            #plt.scatter(self.xx, self.data)
            #plt.contour(self.xx, self.yy, self.data)
            fig, ax = plt.subplots()
            if self.z_str == 'bandgap':
                levels=np.linspace(10.1,12.1,6)
                CS = ax.contour(self.xx, self.yy, self.data, levels=levels)
                ax.clabel(CS, inline=True, fontsize=16)
            elif self.z_str == 'koopmans':
                levels=[-0.2,-0.1,0.,0.1,0.2]
                CS = ax.contourf(self.xx, self.yy, self.data, levels=levels)
                plt.colorbar(CS)
            else:
                print('Error! Coutour plot method not specified')
            #ax.set_title('Simplest default with labels')
        # plot setting
        plt.xlabel(self.xlabel) # x label 
        #plt.ylabel(self.ylabel) # ylabel
        if self.xlim:plt.xlim(self.xlim) # if need to set xlim or ylim: set limits
        if self.ylim:plt.ylim(self.ylim) # set limits
        if self.y_str=='lattice_para':
            pass # done previously
        else:
            plt.title(self.title) # add title
            plt.ylabel(self.ylabel) # y label, from y_str
        # save
        plt.tight_layout()
        figname="%scontour-%s_%s_%s.pdf" % (self.header,self.z_str, self.x_str, self.y_str)
        print('save figure:%s/%s'% (os.environ['PWD'],figname))
        plt.savefig(path+figname,dpi=600)
        #plt.close()

