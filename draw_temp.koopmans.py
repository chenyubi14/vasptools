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

# In[2]:
# put data and information here



x=np.array([0.0,0.2,0.4])
xlabel='AEXX'
y=np.array([12.22698,12.64881,13.07626])
ylabel='Koopmans'
ylabelunit=' (eV)'
legend='$E(N+1.00)-E(N)$'
z=np.array([12.267772,12.766784,13.273477])
comment='initial_HFSCREEN=0.2_AEXX=0.405'
title='perfect fitAEXX=-0.10'
plt.plot(x,z,'o', label='$dN*\epsilon_{lu}(N)$')

# In[3]: plot
#phase='w-BeO'  #phase='r-BeO'
# another legend
## split comment: HFSCREEN=0.2_AEXX=0.405(comment0) in legend, fitAEXX=0.153(comment1) in title
#comments=comment.split('fit')
#comment0=comments[0]
#comment1='fit'+comments[1]
#legend = legend+' '+comment0.replace('_',' ')
#xtext=np.linspace(x[0],x[-1],6)[1]
#ytext=list(y)+list(z) # put y and z values together in the same list
#ytext=np.linspace(min(ytext),max(ytext),6)[1]
#plt.text(xtext,ytext,'initial '+comment0.replace('_',' '))
#title2 = title+' '+comment1
#plt.plot(x, y, 'o', label=legend)
#plt.legend()
#plt.title(title2) # add title
#plt.xlabel(xlabel) # x label 
#plt.ylabel(ylabel+ylabelunit) # y label
#figname= 'graph_%s_%s_%s_%s.jpg' % (title.replace(' ','_'),comment, ylabel, xlabel)
#print('\tfigure saved as \n%s/%s' % (os.environ['PWD'],figname))
#plt.savefig(figname)
#plt.close()


#legend = legend+' '+comment.replace('_',' ')
# write comment with plt.text
xtext=np.linspace(x[0],x[-1],10)[1]
ytext=list(y)+list(z) # put y and z values together in the same list
ytext=np.linspace(min(ytext),max(ytext),6)[1]
plt.text(xtext,ytext,comment.replace('_',' '))

#title2 = title+' '+comment1
plt.plot(x, y, 'o', label=legend)
plt.legend()
plt.title(title) # add title
plt.xlabel(xlabel) # x label 
plt.ylabel(ylabel+ylabelunit) # y label
plt.tight_layout()

figname= 'graph_%s_%s_y%s_x%s.pdf' % (title.replace(' ','_'),comment.replace(' ','_'), ylabel, xlabel)
print('\tfigure saved as \n%s/%s' % (os.environ['PWD'],figname))
plt.savefig(figname,dpi=600)
plt.close()
