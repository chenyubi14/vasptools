#!/usr/bin/env python
# coding: utf-8

# In[1]:
import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
path = os.environ['PWD'] + '/'

from class0_functions1 import find_files, savedata,parent_folder
from class1_read import read_file_values, rotate_read


# deal with intermediate steps for data like ENCUT, AEXX, dielectric constants, ...
#Freysoldt correction!!!


# In[2]:

#read_fil=read_file_values(pwd)
#read_fil.locpot2freysoldt_correction()

class intermediate:
    def __init__(self, folder):
        '''
        self.folder: the working folder
        '''
        if folder[-1] != '/':
            folder=folder+'/' # make sure the folder has correct form: ....../INCAR
        self.folder=folder
        hierdirecs = parent_folder(self.folder, hier=10)  # to find 1st-class materials folder
        assert len(hierdirecs) <= 2, 'You are in the son folder. Go back to the parent folder! Otherwise another son folder will be created'
        self.hierdirecs = hierdirecs
        self.hier1direc = self.hierdirecs[0]    # 1st-class folder Eg. '/home/yubi/work/berylliumO/'
        self.hier2direc = self.hierdirecs[1]    # 2nd-class folder Eg. '/home/yubi/work/berylliumO/wurtzite_2_Vo_HSE/'
        self.sources_direc= self.hier1direc+'sources/' # Eg. '/home/yubi/work/berylliumO/sources/'
    
    def rotate_read_folders(self, x_str, y_str, header, var):
        '''
        dielectric constants: rotate_f.rotate('AEXX', 'eps', 'eps','AEXX') because y_str=header ='eps', x_str = var = 'AEXX'
        highest occupied energy: rotate_f.rotate('AEXX', 'energyHO', 'defect','AEXX') because y_str='energyHO', header='defect', x_str=var='AEXX'
        '''
        rotate_f=rotate_read(self.folder)
        xx,yy=rotate_f.rotate(x_str, y_str, header, var)
        print(xx, yy)
        # ...

