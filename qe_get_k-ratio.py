#!/usr/bin/env python

import numpy as np
import argparse


parser = argparse.ArgumentParser(description='Get k grid ratio')
parser.add_argument('--inqe',type=str,default='in.qe',
        help='in.qe filename')
parser.add_argument('--v',type=bool,default=False,
        help='verbosity, True(1) or False(0)')
args = parser.parse_args()
inqe = args.inqe
verb = args.v


with open(inqe, 'r') as f:
    lines = f.readlines()
    for i in range(len(lines)):
        if 'CELL_PARAMETERS' in lines[i]:
            lines = lines[i+1:i+4]
            break
a = lines[0].split()
b = lines[1].split()
c = lines[2].split()

if len(a) > 3:
    a = a[1:]
    b = b[1:]
    c = c[1:]

a = np.array(a).astype(float)
b = np.array(b).astype(float)
c = np.array(c).astype(float)

a = 1/np.linalg.norm(a)
b = 1/np.linalg.norm(b)
c = 1/np.linalg.norm(c)
arr = np.array([a,b,c])

minim = np.min(arr)
new_arr = arr / minim
if verb:
    print('raw:    %s %s %s' % tuple(arr) )
    print('scaled: %s %s %s\nrounded:' % tuple(new_arr) )
new_arr = np.round(new_arr)
new_arr = np.int64(new_arr)
print('%s %s %s' % tuple(new_arr) )
