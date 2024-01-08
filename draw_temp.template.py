#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
import os
import matplotlib.ticker as tck

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

## x
x=np.array([])
## y
y=np.array([])

plt.plot(x, y, label='', 
        marker='o', markersize=6, 
        linewidth=3, linestyle='-') # put legend here
#shapes=['o','*','s'] # marker shape
#markersize=[6,10,6]


## settings
xlabel=''
xlabelunit=''
ylabel=''
ylabelunit=''
title=''
comment=''


## legend, title ,label
plt.legend()
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
## limits
xlimit=None
ylimit=None
if xlimit:
    plt.xlim(xlimit)
if ylimit:
    plt.ylim(ylimit)


ax=plt.gca()
### minor locator
labeltype=0
if labeltype == 0:
    ## x/y both are normal scale
    ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
    ax.yaxis.set_minor_locator(tck.AutoMinorLocator())
    #ax.yaxis.set_major_locator(tck.MultipleLocator(5))
else:
    ## y log scale, x normal scale
    ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
    plt.yscale('log')
    ax.yaxis.set_minor_locator( tck.LogLocator(numticks=999,subs='auto') )

ax.spines['left'].set_linewidth(1.5)
ax.spines['right'].set_linewidth(1.5)
ax.spines['top'].set_linewidth(1.5)
ax.spines['bottom'].set_linewidth(1.5)
ax.tick_params(top=True,bottom=True,right=True,left=True,direction='in')
ax.tick_params(which='minor', top=True,bottom=True,right=True,left=True,direction='in')
#ax.grid(True)
#ax.fill_between(x,-vbmregion, 0, color='skyblue')
#ax.yaxis.get_offset_text().set_fontsize(25)


### optional
#plt.xticks([0,1,2])
#plt.yticks([0,1,2])
#plt.vlines(0,ylimits[0],ylimits[1],linewidth=3.0,color='black')
#plt.hlines(value,xlim1, xlim2, colors='black',linewidth=5)
#plt.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
#plt.text(x,y,'text')

#save
plt.tight_layout()
figname= 'graph_%s_%s_y%s_x%s.pdf' % (title.replace(' ','_'),comment.replace(' ','_'), ylabel.replace(' ','_'), xlabel.replace(' ','_'))
print('\tremotesync \n%s' % (figname))
plt.savefig(figname,dpi=600, transparent=True)
plt.close()



#In[]:
## remove all axis related objects: spines, xlabel,ylabel, ticks, locators
#ax.axis('off')

## remove one axis
#ax.get_xaxis().set_visible(False) # remove x axis 
#ax.get_yaxis().set_visible(False) # remove x axis 

## a different axis independent of the original axis
#ax2=ax.twinx() # will appear as a different axis on the right spine
#ax2.set_yticks(yticks) 

## spines: the boundary of the plot
### set length 
#ax.spines['left'].set_linewidth(1.5) # thickness of the boundary
#ax.spines['right'].set_linewidth(1.5)
#ax.spines['top'].set_linewidth(1.5)
#ax.spines['bottom'].set_linewidth(1.5)
### make it disappear
#### make y axis empty
#ax.spines['right'].set_visible(False) # make the right spine disappear
#ax.spines['left'].set_visible(False) # make the left spine disappear
#### make x axis empty
#ax.spines['top'].set_visible(False)
#ax.spines['bottom'].set_visible(False)

## ticks
### make disappear
#ax.set_yticks([]) # remove y ticks
#ax.tick_params(length=0) # set the length of main ticks to be zero for both x,y axis
### set tick's labels
##plt.xticks(np.linspace(xlimits[0],xlimits[1], 1))
##plt.yticks(np.lin(-20, 25, 5))
### method to change x tick labels
#labels=[item.get_text() for item in ax.get_xticklabels()]
#labels[1] = 'abc'
#ax.set_xticklabels(labels)
### method to change x tick labels
#xticks = range(1,2*num_defect,2)
#ax.set_xticks(xticks) # make sure the tick occupies the right position, with the right interval
#ax.set_xticklabels(defecttypes, fontsize=BIGGER_SIZE) # changes the x tick labels


## major locator
#### remove x major locator
#plt.tick_params(axis='x', length=0) # length=0 makes the major tick disappear, works the same as next line, which='minor', width=1.5,, color='k', right=True
### remove x major locators, add y major locators on the right.
#ax.tick_params(top=False,bottom=False,right=True,left=True) # whether the major ticks will appear
### scale 
#import matplotlib.ticker as tck
#MultipleLocator is in matplotlib.ticker
#ax.xaxis.set_major_locator(tck.MultipleLocator(0.5)) # MultipleLocator the interval between y axis major ticks, like 0-0.5-1-1.5-...
#ax.yaxis.set_major_locator(tck.MultipleLocator(5)) # MultipleLocator the interval between y axis major ticks, like 0-5-10
#ax.yaxis.set_major_locator(tck.AutoLocator())
### thickness
#plt.tick_params(axis='x', width=3, length=7,  labelsize=15) 
#plt.tick_params(axis='y', width=2, length=4) # thickness of major labels # length is how far the locators extend out , labelsize=15


## minor locator
#import matplotlib.ticker as tck
### set thickness, color, make right-hand-side appear
#plt.tick_params(axis='y', which='minor', width=1., length=2, color='k', right=True) # set minor thinkness, interval
### make minor appear
#ax.yaxis.set_minor_locator(tck.AutoMinorLocator()) # have y minor ticks
#ax.xaxis.set_minor_locator(tck.AutoMinorLocator()) # if don't want x ticks, don't need this line
### log scale x axis
#ax.xaxis.set_major_locator(tck.LogLocator(numticks=999))
#ax.xaxis.set_minor_locator(tck.LogLocator(numticks=999,subs=(.2,.4,.6,.8)))

### example: x scale normal, y scale log
## make the direction of locator inward
#ax.tick_params(top=True,bottom=True,right=True,left=True,direction='in',length=5,width=1.2)
#ax.tick_params(which='minor', top=True,bottom=True,right=True,left=True,direction='in',width=1.0)
#ax.xaxis.set_minor_locator(tck.AutoMinorLocator())
#ax.set_yscale('log')
#ax.yaxis.set_minor_locator(tck.LogLocator(numticks=999,subs='auto'))
#ax.yaxis.set_minor_locator(tck.LogLocator(numticks=999,subs=(.1,.2,.3,.4,.5,.6,.7,.8,.9)))


### legend
#### size of legend
#plt.legend(borderpad=2)
#### legend in four columns: ncol=4
#plt.legend(ncl=4)
#### make legend not transparent
#plt.legend(framealpha=0.)
#### add description for each row (not column!)
#handles, labels = ax.get_legend_handles_labels()
#handles = [plt.plot([],marker="", ls="")[0]]*2 + list(flip(handles,3)) # an empty handle
#labels=['Calc:','Expr:'] + list(flip(labels,3)) # labels for empty handle, used as row category
#leg=plt.legend(handles, labels, loc='lower center', ncol=4, columnspacing=0.5)
## make the empty column ['Calc:','Expr:'] taken no space
#for vpack in leg._legend_handle_box.get_children()[:1]:
#    for hpack in vpack.get_children():
#        hpack.get_children()[0].set_width(0)
# legend fill columns by default, fill by row first
import itertools
def flip(items, ncol):
    return itertools.chain(*[items[i::ncol] for i in range(ncol)])
#leg = ax.get_legend()
#handles, labels = ax.get_legend_handles_labels()
#ax.legend(flip(handles, 3), flip(labels, 3), loc='lower center', ncol=3, columnspacing=0.5)

### offset text
##    cb.ax.yaxis.get_offset_text().set_visible(False)
##    cb.ax.text(0.5, -0.1, '1e4', va='bottom', ha='center', size=6)



#In[]:
## draw a region
#x=np.linspace(0,largestx,50)
#ax.fill_between(x,-vbmregion, 0, color='skyblue') #defectenergyCBM,defectenergyCBM+cbmregion)
#ax.fill_between(x,bandgap,bandgap+cbmregion,color='skyblue') #'#1f77b4') # defectenergyVBM,defectenergyVBM-vbmregion)

## draw a horizontal line
#ax.hlines(defect_fermilevel[0],0,2*num_defect,colors='black',linewidths=5)

## draw a vertical line
#plt.axvline(x[-1]/2, color='k',ls='--') # a vertical line to separate spin up and spin down, ls is linestyle

## draw an arrow
#head_width=0.2 #0.1
#plt.arrow(0, bandgap/3,0,bandgap/3,width=width,head_width=head_width,color='black')

## grid
#plt.grid(linestyle='dashed')
#ax.grid(True)
#ax.grid(axis='x')

