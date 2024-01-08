import numpy as np
import os
import sys
sys.path.append(os.environ['SCRIPT'])
import shutil
import pymatgen
from pathlib import Path
from class0_functions1 import parent_folder

# In[0]:
def Representsfunc(s, func=int):
    try: 
        func(s)
        return True
    except ValueError:
        return False

def get_printformula(dict_elementformula):
    string=''
    elements = dict_elementformula.keys()
    for ele in elements:
        num=int(dict_elementformula[ele]) # make sure the formula is an integer
        num=str(num)
        string = string+ele+num # Be2O2
    return string

def struc2poscar(struc, folder, fname='POSCAR', selective_dynamics=False):
    folder = Path(folder)
    assert type(struc)==pymatgen.core.structure.Structure, 'You should enter struc with type=pymatgen.core.structure.Structure'
    #if folder[-1] != '/':
    #    folder=folder+'/' # make sure the folder has correct form
    #selective_dynamics=[[True]*3]*(struc.num_sites-1) # True: yes move
    #selective_dynamics.insert(0,[False]*3) # fix the first atom to avoid translational motion
    if selective_dynamics:
        selective_dynamics = [[True]*3]*(struc.num_sites)
        struc.to(filename=str(folder/fname), selective_dynamics=selective_dynamics, fmt='poscar')
    else:
        struc.to(filename=str(folder/fname), fmt='poscar')

def hcf(*x): # calculate the greatest common devisor
    smaller=min(x)
    for i in reversed(range(1,smaller+1)):
        if list(filter(lambda j: j%i!=0,x)) == []:
            return i


def lcm(*x): #计算最小公倍数
    greater=max(x)
    while True:
        if list(filter(lambda i: greater%i!=0,x)) == []:
            return greater
        greater+=1
  
def generate_KPATH(kpathfolder=''):
    '''
    kpathfolder: the folder of KPATH
    generate KPOINTS from KPATH
    '''
    pwd = os.getcwd() + '/'
    if kpathfolder!='':
        print('use KPATH from folder %s to generate KPOINTS' % kpathfolder)
    else:
        hier1direc = parent_folder(pwd, hier=10)[0] # 1st-class material folder
        kpathfolder = hier1direc + 'sources/'

    # file KPATH specifies band information
    with open(kpathfolder+'KPATH','r') as f1: 
        lines = f1.readlines()
        N = int(lines[0].split()[2]) # split a kpoint path gamma->A into N segments
        Vec = []
        KPOINTS = np.array([])
        for i in range(1,len(lines)):
            splitted = np.array(lines[i].split())
            Vec.append(splitted[0]) # ['G', 'M', 'K', 'G', 'A', 'L', 'H', 'A']
            KPOINTS = np.append(KPOINTS, splitted[1:].astype(float)) # have to read as numbers
        KPOINTS = np.reshape(KPOINTS,(-1,3))
        # make comments for the special KPOINTS, no comment for the interpolated points
        commentkp=[Vec[0]] # ['G']
        for i in range(1,len(Vec)):
            comment_i = ['']*(N-1) # ['','',...,'']
            comment_i.append(Vec[i]) # ['','',...,'','M']
            comment_i.append(Vec[i]) # ['','',...,'','M','M']
            commentkp=commentkp+comment_i # ['G', '','',..., '','M','M']
        commentkp = commentkp[:-1]
        print('comment length',len(commentkp))
    #kpath = np.array([])
    kpath = np.linspace(KPOINTS[0], KPOINTS[1], N+1) # length of kpath = N+1, there are N-1 mid points 
    for i in range(1, len(Vec)-1): # stack different paths together
        kpath = np.vstack((kpath, np.linspace(KPOINTS[i], KPOINTS[i+1], N+1))) # length of kpath = N+1, there are N-1 mid points 
        # need to double the intermediate points. No special attention to take like the below
    #kpath = np.linspace(KPOINTS[0], KPOINTS[1], N+1) # length of kpath = N+1, there are N-1 mid points 
    #for i in range(1, len(Vec)-1): # stack different paths together
    #kpath = np.vstack((kpath, np.linspace(KPOINTS[i], KPOINTS[i+1], N+1)[1:] )) # [1:] omits the same points, but now don't omit
    nkpts = len(kpath)

    with open(pwd+'IBZKPT','r') as f2: # read BZ kpoints
        lines = f2.readlines()
        nkibz = int(lines[1])
        nktot = nkibz + nkpts

    # Write KPOINTS with new kpoints
    with open(pwd+'KPOINTS','w') as f3:
        f3.write("Automatically generated mesh following KPATH\n%s\nReciprocal lattice\n" % (nktot))
        write_content = ''.join(lines[3: nkibz+3]) # join the list to a string
        f3.write(write_content)
        # add zero share to each kpoint
        kpath = np.round(kpath,8)
        kpath = np.c_[kpath, np.zeros(len(kpath))]
        kpath = np.c_[kpath, commentkp]
        for i in range(nkpts):
            f3.write("%s %s %s %s\t\t%s\n" % tuple(kpath[i]))

