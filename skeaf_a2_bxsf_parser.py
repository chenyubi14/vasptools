# -*- coding: utf-8 -*-
"""
Created on Thu Sep 24 13:40:39 2020

@author: lllan
"""

import re
import numpy as np
import sys


class BxsfParser:
    def __init__(self, infile = "in.bxsf"):
        
        self.bxsf = infile
        
        rf = open(self.bxsf)
        self.data = rf.read()
        rf.close()
        
        self.reclat = None
        self.numBands = None
        self.origin = None 
        self.nkfs_dim = None
        self.nkfs = None
        
        self.nk_dim = None
        self.nk = None
        
        self.bandLabels = None
        
        self.bandData = None
        
        self.listExtra = None
        
        self.bandEnergy = None
        self.bandEnergy_fs = None
        self.bands = None
        self.parse_bxsf()
        
    def parse_bxsf(self):
        self.fermi_energy = float( re.findall("Fermi Energy:\s(.*)", self.data)[0] )
        self.numBands = int(re.findall("BEGIN\_BLOCK\_BANDGRID\_3D\n.*\n.*\n\s*(\d*)", self.data)[0])
        
        self.origin = re.findall("BEGIN\_BLOCK\_BANDGRID\_3D\n.*\n.*\n.*\n.*\n(.*)", self.data)[0].split()
        self.origin = np.array([float(x) for x in self.origin])
        
        self.reclat = re.findall("BEGIN\_BLOCK\_BANDGRID\_3D\n.*\n.*\n.*\n.*\n.*\n" + 3 * "\s*(.*)\s*\n", self.data)[0]
        self.reclat = np.array([[float(y) for y in x.split()] for x in self.reclat ])

        raw_nkfs = re.findall("BEGIN\_BLOCK\_BANDGRID\_3D\n.*\n.*\n.*\n(.*)", self.data)[0]
        self.nkfs_dim = np.array([int(x) for x in raw_nkfs.split()])
        self.nkfs = np.product(self.nkfs_dim)
        
        self.nk_dim = np.array([int(x) - 1 for x in raw_nkfs.split()])
        self.nk = np.product(self.nk_dim)
        self.bandLabels = re.findall("BAND\:\s*(.*)", self.data)
        
        
        self.kpoints_fs = np.zeros(shape = [self.nkfs_dim[0]*self.nkfs_dim[1]*self.nkfs_dim[2],3])
        #self.kpoints_fs2 = np.zeros(shape = [self.nkfs_dim[0]*self.nkfs_dim[1]*self.nkfs_dim[2],3])
        
        bandDataDim = list(self.nkfs_dim)
        bandDataDim.insert(0,self.numBands)
        self.bandData = np.zeros(shape = bandDataDim)
        #self.bandData2 = np.zeros(shape = bandDataDim)
        self.kData = np.zeros(shape = list(self.nkfs_dim))
        
        self.bandEnergy_fs = np.zeros(shape = [self.nkfs_dim[0]*self.nkfs_dim[1]*self.nkfs_dim[2],self.numBands])
        #self.bandEnergy_fs2 = np.zeros(shape = [self.nkfs_dim[0]*self.nkfs_dim[1]*self.nkfs_dim[2],self.numBands])
      
        
        split_band = self.data.split('BAND:')[1:]
        for iband in range(self.numBands):
            counter = 0
            #expression = "BAND\:\s*" + self.bandLabels[iband] + "[\s\S]*(?=BAND\:\s*"+ self.bandLabels[iband+1]+ "\n)"
            #self.bandEnergy = re.findall(expression, self.data)[0].split()
            self.bandEnergy = split_band[iband].split()
            self.bandEnergy.pop(0)
            if ( self.numBands -1 == iband  ):
                self.bandEnergy.pop(-1)
                self.bandEnergy.pop(-1)
            self.bandEnergy=np.array(self.bandEnergy).astype(float)
            self.listExtra = []
            self.bandData[iband,:,:,:] = self.bandEnergy.reshape(self.nkfs_dim)
            self.bandEnergy_fs[:,iband] = self.bandEnergy
            self.kpoints_fs2_x = np.repeat(np.linspace(0,1,self.nkfs_dim[0]),self.nkfs_dim[1]*self.nkfs_dim[2]).flatten()
            self.kpoints_fs2_y = np.repeat([np.repeat( np.linspace(0,1,self.nkfs_dim[1]) , self.nkfs_dim[2])],self.nkfs_dim[0] ,axis=0).flatten()
            self.kpoints_fs2_z = np.repeat([ np.linspace(0,1,self.nkfs_dim[2]) ], self.nkfs_dim[0]*self.nkfs_dim[1],axis=0).flatten()
            self.kpoints_fs = np.stack([self.kpoints_fs2_x, self.kpoints_fs2_y, self.kpoints_fs2_z]).T

            #counter = 0
            #for i in range(self.nkfs_dim[0]):
            #    for j in range(self.nkfs_dim[1]):
            #        for k in range(self.nkfs_dim[2]):
            #            if i == self.nkfs_dim[0]-1 or j == self.nkfs_dim[1]-1 or k == self.nkfs_dim[2]-1:
            #                 self.listExtra.append(counter)
            #            self.bandData[iband,i,j,k] = self.bandEnergy[counter]
            #            self.bandEnergy_fs[counter, iband] = self.bandEnergy[counter]
            #            self.kpoints_fs[counter,:] = np.array([(i)/(self.nkfs_dim[0]-1),(j)/(self.nkfs_dim[1]-1),(k)/(self.nkfs_dim[2]-1)])
            #            counter += 1
            #self.kpoints = np.delete(self.kpoints_fs,self.listExtra, axis = 0)
            #self.bands = np.delete(self.bandEnergy_fs,self.listExtra , axis = 0)
            
            selected_delete = np.unique(np.where(self.kpoints_fs==1)[0]) #np.where(self.kpoints_fs2==1).sum(axis=1) 
            self.kpoints = np.delete(self.kpoints_fs,selected_delete,axis=0) #np.delete(self.kpoints_fs,self.listExtra, axis = 0)
            self.bands = np.delete(self.bandEnergy_fs,selected_delete,axis=0) #np.delete(self.bandEnergy_fs,self.listExtra , axis = 0)
            #assert np.allclose(self.kpoints_fs,self.kpoints_fs2) and np.all(self.bandEnergy_fs==self.bandEnergy_fs2) and np.all(self.bandData==self.bandData2)
            #assert np.allclose(self.kpoints,self.kpoints2) and np.all(self.bands==self.bands2)
 
