#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
import os
import argparse
import sys

parser = argparse.ArgumentParser(description='input BS details')
parser.add_argument('--datafil', type=str, default='bands.dat.gnu',
        help='data file name')
parser.add_argument('--fermi', metavar='f', type=float, default=None,
        help='directly input fermi level value, ')
parser.add_argument('--outp', type=str, default=None,
        help='get fermi level from ../scf/out.qe file')
parser.add_argument('--inp', type=str, default='in.qe',
        help='get kpath from ./in.qe file')
parser.add_argument('--tick', metavar='t', type=str, nargs='+',default=None,
        help='kpath point names used as ticks')

args = parser.parse_args()
datafil = args.datafil
fermi = args.fermi
outp = args.outp
inp = args.inp
tick = args.tick
color = 'royalblue'
#infofil = 'in.qe'
## can read in.qe to get segment points, also need a way to add point name

def get_fermi(fermi, outp):
    if fermi==None and outp == None:
        print('Error! Need fermi level. Either specify --fermi or --outp\n python $SCRIPT/qe_06* --fermi 0.0 --outp ../scf/out.qe')
        sys.exit()
    elif fermi==None and os.path.isfile(outp):
        import re
        patt = re.compile('the Fermi energy is (.*) ev')
        with open(outp, 'r') as f:
            lines = f.readlines()
        fermi = patt.search(''.join(lines))
        fermi = fermi.group(1)
        print('Fermi level from out.qe is %s eV' % (fermi) )
        fermi = float(fermi)
    elif fermi:
        print('Fermi level is %s eV' % (fermi)  )
    else:
        print('Error! file %s does not exist' % outp )
        sys.exit()
    return fermi

def get_kticks(tick, inp, xarr):
    if os.path.isfile(inp):
        with open(inp, 'r') as f:
            lines = f.readlines()
        for i, l in enumerate(lines):
            if 'K_POINTS' in l:
                num_kp = int(lines[i+1])
                kpoints = lines[i+2:i+2+num_kp]
                break
    else:
        print('Error! file %s does not exist' % inp)
    ## get tick_positions in xarr
    seg_info = [ kp.split()[3] for kp in kpoints ]
    print('kpath info from %s is %s' % (inp, seg_info))
    seg_info = np.array(seg_info).astype(int)
    seg_info = np.cumsum(seg_info)
    seg_info = np.insert(seg_info, 0, 0)
    seg_info = seg_info[:-1]
    seg_info = xarr[seg_info]
    ## get tick_label
    labels = []
    if tick:
        labels = tick
        assert len(labels) == len(seg_info), '--tick does not match the number of points in kpath'
    elif tick == None:
        for kp in kpoints:
            splitted = kp.split()
            if len(splitted) <= 4:
                print('No information about ticks in in.qe, empty ticks')
                print('\tuse --tick $\Gamma$ M K  --inp in.qe')
                labels=[None] * len(seg_info)
                break
            else:
                labels.append(splitted[-1].split('!')[-1])
    print('%s %s' % ( labels, seg_info) )
    return seg_info, labels

def get_length(fil):
    with open(fil, 'r') as f:
        lines = f.readlines()
    for i in range(len(lines)):
        if lines[i].strip(' ') == '\n':
            return i



SMALL_SIZE = 16
MEDIUM_SIZE = 18
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=MEDIUM_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=BIGGER_SIZE)  # fontsize of the figure title
plt.figure(figsize=(7,5))



fermi = get_fermi(fermi, outp)
length = get_length(datafil)
data = np.loadtxt(datafil)
x=data[:,0]
y=data[:,1]
## shift reference to fermi level
y = y - fermi
#x=x.reshape((-1,length))
#y=y.reshape((-1,length))

begin=0;end=length
xarr = x[begin:end]
tick_position, tick_label = get_kticks(tick, inp, xarr)

plt.plot(x[begin:end], y[begin:end], 
         color = color,
         label = '')
for i in range(1, int(len(x)/length) ):
    begin = end
    end = begin + length
    plt.plot(x[begin:end], y[begin:end], 
             color = color,
             )


## add ticks and dashed lines
for tick in np.sort(tick_position)[1:-1]:
    plt.axvline(x=tick,c ='grey',ls='--')
plt.xticks(tick_position,tick_label)

#####################################################################
labeltype=0 # no labels
if labeltype == 0:
    #plt.legend( loc='upper right',
    #            #framealpha=1.0,
    #            #bbox_to_anchor=(0.11,0.3),
    #            )   
    ylabel='Energy [eV]'
    title='Band Structure'
    import matplotlib.ticker as tck
    ax = plt.gca()
    #ax.yaxis.set_major_locator(tck.MultipleLocator(1))
    ax.yaxis.set_major_locator(tck.AutoLocator())
    ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.tick_params(
            top=False,bottom=False,right=True,left=True,
            direction='in',width=1.2)
    ax.tick_params(which='minor',
            top=False,bottom=False,right=True,left=True,
            direction='in',width=1.0)
#####################################################################

plt.title(title) # add title
plt.ylabel(ylabel) # y label

# limits
xlimit=[min(xarr), max(xarr)+1e-4]
ylimit=None
#ylimit=[-5,9]
ylimit=[-1.5,1.5]
if xlimit:
    plt.xlim(xlimit)
if ylimit:
    plt.ylim(ylimit)

#save
plt.tight_layout()
figname= 'qe_bandstructure.pdf'  
print('Saved %s' % (figname))
plt.savefig(figname, dpi=150, transparent=True)
plt.close()



#import itertools
#def flip(items, ncol):
#    return itertools.chain(*[items[i::ncol] for i in range(ncol)])

