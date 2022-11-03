# In[1]:
import numpy as np
import matplotlib.pyplot as plt
import os

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
plt.figure()

# x
x=np.array([])
# y
y=np.array([])
# z
z=np.array([])

plt.plot(x, y, 'o',  label="") # put legend here
plt.plot(x, z, 'o',  label="") # put legend here


# settings
xlabel=''
xlabelunit=''
ylabel=''
ylabelunit=''

title=''
comment=''

# legend, title ,label
plt.legend()
plt.title(title) # add title
plt.xlabel(xlabel+xlabelunit) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()

# limits
plt.xlim([])
plt.ylim([])

#save
figname= 'graph_%s_%s_y%s_x%s.pdf' % (title.replace(' ','_'),comment.replace(' ','_'), ylabel.replace(' ','_'), xlabel.replace(' ','_'))
print('\tremotesync \n%s' % (figname))
plt.savefig(figname,dpi=600)
plt.close()
#plt.show()

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
#ax.tick_params(length=0) # set the length of main ticks to be zero for both x,y axis # not very sure what this means now
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
#ax.xaxis.set_major_locator(MultipleLocator(0.5)) # MultipleLocator the interval between y axis major ticks, like 0-0.5-1-1.5-...
#ax.yaxis.set_major_locator(MultipleLocator(5)) # MultipleLocator the interval between y axis major ticks, like 0-5-10
### thickness
#plt.tick_params(axis='x', width=3, length=7,  labelsize=15) 
#plt.tick_params(axis='y', width=2, length=4) # thickness of major labels # length is how far the locators extend out , labelsize=15

## minor locator
#import matplotlib.ticker as tck
### make minor appear
#ax.yaxis.set_minor_locator(tck.AutoMinorLocator()) # have y minor ticks
#ax.xaxis.set_minor_locator(tck.AutoMinorLocator()) # if don't want x ticks, don't need this line
### set thickness, color, make right-hand-side appear
#plt.tick_params(axis='y', which='minor', width=1., length=2, color='k', right=True) # set minor thinkness, interval


#In[]:
## draw a region
#x=np.linspace(0,largestx,50)
#ax.fill_between(x,-vbmregion, 0, color='skyblue') #defectenergyCBM,defectenergyCBM+cbmregion)
#ax.fill_between(x,bandgap,bandgap+cbmregion,color='skyblue') #'#1f77b4') # defectenergyVBM,defectenergyVBM-vbmregion)

## draw a horizontal line
#ax.hlines(defect_fermilevel[0],0,2*num_defect,colors='black',linewidths=5)
#ax.hlines(defect_fermilevel[-1],0,2*num_defect,colors='black',linewidths=5)

## draw a vertical line
#plt.axvline(x[-1]/2, color='k',ls='--') # a vertical line to separate spin up and spin down, ls is linestyle

## draw an arrow
#head_width=0.2 #0.1
#plt.arrow(0, bandgap/3,0,bandgap/3,width=width,head_width=head_width,color='black')

## grid
#ax.grid(True)
