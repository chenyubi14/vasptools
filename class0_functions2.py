import numpy as np
import os
import sys
sys.path.append(os.environ['SCRIPT'])
import shutil
import pymatgen
from pathlib import Path


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

def struc2poscar(struc, folder, fname='POSCAR'):
    folder = Path(folder)
    assert type(struc)==pymatgen.core.structure.Structure, 'You should enter struc with type=pymatgen.core.structure.Structure'
    #if folder[-1] != '/':
    #    folder=folder+'/' # make sure the folder has correct form
    #selective_dynamics=[[True]*3]*(struc.num_sites-1) # True: yes move
    #selective_dynamics.insert(0,[False]*3) # fix the first atom to avoid translational motion
    selective_dynamics = [[True]*3]*(struc.num_sites)
    struc.to(filename=str(folder/fname), selective_dynamics=selective_dynamics, fmt='poscar')

def hcf(*x): # calculate the greatest common devisor
    smaller=min(x)
    for i in reversed(range(1,smaller+1)):
        if list(filter(lambda j: j%i!=0,x)) == []:
            return i


def lcm(*x): #calculate the least common multiple
    greater=max(x)
    while True:
        if list(filter(lambda i: greater%i!=0,x)) == []:
            return greater
        greater+=1
  
