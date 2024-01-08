#!/usr/bin/env python
import sys
import os
import argparse
import numpy as np
sys.path.append(os.environ['SCRIPT'])
from class2_update_input import change_input_files


#print('Edit INCAR to get On-site Coulomb interaction U parameter')
change_files=change_input_files('./')


parser = argparse.ArgumentParser(description='LDAUL setup. Enter magnitude and a string like (n n d f n) ')
parser.add_argument('mode', type=int, nargs=1, choices=[0,1,2],
        help='enter 0/1/2 for reference/self-consistent/non-self-consistent',)
parser.add_argument('magnitude', type=float, nargs=1,
        help='Shift the atomic potential in spin up/down channel rigidly by this amount',)
parser.add_argument('sel', type=str, nargs='+',choices=['n','d','f'],
        help='n for s/p means not add U to atoms, d means for atoms with d orbitals, f means for atoms with f orbitals')
args = parser.parse_args()
mode = args.mode[0]
sel = args.sel
magnitude = args.magnitude[0]


def convert(sel, magnitude):
    change = { 'n':'-1', 'd':'2', 'f':'3' }
    ldaul = ''
    ldauu = ''
    for s_i in sel:
        #print(s_i)
        ldaul = ldaul + change[s_i] + ' '
        if s_i != 'n':
            ldauu = ldauu + str(magnitude) + ' '
        else:
            ldauu = ldauu + '0.0 '
    return  ldaul, ldauu


ldaul, ldauu = convert(sel, magnitude)
ldauj = ldauu
edits = {'LDAU':'T','LDAUTYPE':3,
        'LDAUL':ldaul,'LDAUU':ldauu, 'LDAUJ':ldauj, 
        'LDAUPRINT':1, 'LASPH':'T', 
        'LORBIT':11, }
if mode == 1:
    nsc = {'ICHARG':1,'LWAVE':'F', 'LCHARG':'F', 'ISIF':2, 'NSW':0, 'IBRION':-1}
    edits = {**edits, **nsc}
elif mode == 2:
    #print('non self consistent!!')
    nsc = {'ICHARG':11, 'NELMIN':10, 'LWAVE':'F', 'LCHARG':'F', 'ISIF':2, 'NSW':0, 'IBRION':-1}
    edits = {**edits, **nsc}


change_files.incar_change(edits, popkey=[]) 
