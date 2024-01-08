#!/usr/bin/env python
import numpy as np
import sys


## load data
#if len(sys.argv) < 2:
#	print('Error! Enter the folders')
#	sys.exit()
print('Will plot the two columns in .dat or .txt file.')


folders = ['.'] #sys.argv[1:]
filename = 'band.plot.dat'
fileinfo2 = 'band.conf' ## band.conf for ticklabels, number of q-points in a Path


#### get tick_label, num_qpoint
dict2=np.loadtxt(folders[0] + '/' + fileinfo2, comments='#',dtype=str, delimiter='=') # this one works for data with several columns.
## reformat dict2 to remove spaces
dict2 = [ [dict2[i,0].strip(),dict2[i,1].strip()] for i in range(len(dict2)) ]
dict2=dict(dict2)
### get the number of points in q-Path
num_qpoint=int(dict2['BAND_POINTS']) # number of q-points for each segment
### get tick_label, in order to get where Gamma point is
tick_label=dict2['BAND_LABELS']
tick_label=tick_label.split()
num_seg = len(tick_label) - 1 # number of segments (q-path)




###############################################################################
### In tick_label, get where Gamma point is
tickindex_gamma = [] ## stores the indices for the q-paths with Gamma point
segindex_gamma = [] ## used for which segment to be selected
print(tick_label)
for i,label_i in enumerate(tick_label):
    if 'Gamma' in label_i:
        print('Found Gamma point at i=',i)
        if i == 0:
            ## Gamma at the first tick, right q-path
            tickindex_gamma.append([i, i+2, 1]) # path from tick-0 to tick-1
            segindex_gamma.append(i)
        elif i == len(tick_label)-1:
            ## Gamma at the last tick, left q-path
            tickindex_gamma.append([i, i-2, -1])
            segindex_gamma.append(i-1)
        else:
            ##$ Gamma in the middle
            ### include right q-path
            tickindex_gamma.append([i, i+2, 1]) # path from tick-0 to tick-1
            segindex_gamma.append(i)
            ### include left q-path
            tickindex_gamma.append([i, i-2, -1])
            segindex_gamma.append(i-1)
            #tickindex_gamma.append([i-1, i+1])
            #segindex_gamma.append(i-1)

###############################################################################
## read data
#### data: (num_branch,num_seg,num_qpoint)
data=np.loadtxt(folders[0] + '/' + filename, comments='#') # this one works for data with several columns.
## tried to add 2pi but seems not matching
#x=data[:,0]*2*np.pi
x=data[:,0] #*2*np.pi
y=data[:,1]
x=x.reshape(-1,num_seg,num_qpoint)
y=y.reshape(-1,num_seg,num_qpoint)


def linear_fit(m,n,forback,linear_range=0.2):
    length = len(m)
    assert forback==1 or forback==-1, 'Should be either forward or backward'
    if forback == 1:
        ## selected linear range
        sel = int(length*linear_range)
        coeff = np.polyfit( m[0:sel],n[0:sel],1 )
    elif forback == -1:
        ## selected linear range
        sel = int(length*(1-linear_range))
        coeff = np.polyfit( m[sel:],n[sel:],1 )
    groupV =np.abs( coeff[0] )
    print('\tGroup velocity (km/s) is ', groupV/10)

#### data: (num_branch,num_seg,num_qpoint)
### First loop over num_seg
### Then loop over num_branch, and must select the acoustic phonons
for i in range(len(segindex_gamma)):
    index_i = tickindex_gamma[i] ## [0,2]
    pathlabel_i = tick_label[index_i[0]:index_i[1]:index_i[2]] ## tick_label[0:2:1] or ticklabel[4,2,-1]
    path_i = segindex_gamma[i]
    print('%s' % (pathlabel_i) )
    for j in range(0,3): ## acoustic phonon branch
        print('Acou_branch ', j)
        m = x[j, path_i]
        n = y[j, path_i]
        ### linear fitting
        linear_fit(m,n,forback=index_i[2],linear_range=1/3)

## draw phonon dispersion
#for i,f_i in enumerate(folders):
#    begin=0
#    end=num_qpoint
#    plt.plot(x[begin:end],y[begin:end],)
#    ## use plt.plot with a loop, to avoid plotting a straight line from end to beginning
#    for j in range(1,int(len(x)/num_qpoint)):
#        begin=end
#        end=begin+num_qpoint
#        plt.plot(x[begin:end],y[begin:end],linestyle='-',alpha=1)


