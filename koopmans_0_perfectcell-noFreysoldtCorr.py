#[0]:
import sys
import os
import numpy as np
sys.path.append(os.environ['SCRIPT'])
pwd = os.environ['PWD'] + '/'
#print('In Python',pwd)
from class0_functions1 import read_incar, find_files
from class1_read import read_file_values, rotate_read

# If draw one-dimensional koopmans in a defect, run koopmans1,2,...,7. If draw two-dimensional koopmans in a defect, use draw_out_var3D.py 
# No need to run this koopmans0_*.py file for defects. koopmans7_*.py covered this function entirely.

# This function is used solely for perfect supercells, which does not need freysoldt correction.
# obtain the data for the formula of Koopmans condition: Epos-Eneu+E_ho=0

def extract_charge_comment(folname, formatfolder=0):
    # get the charge between 'defect' and 'e' inside 'defect-1e'
    if formatfolder == 0:
        return float(folname.split('charge')[-1].split('e_')[0]) # return only the charge number

def fit_data(var,x1, y1):
    if var == 'HFSCREEN':
        numberofdata=5
        fity1=y1[:numberofdata]
        fitx1=x1[:numberofdata]
    else:
        fity1=y1
        fitx1=x1
    print('Fit this array %s to 0'%fity1)
    poly=np.polyfit(fity1,fitx1,1)# fitting function
    po=np.poly1d(poly) # poly function
    var_fit=np.round(po(0),3)
    print('Fit %s=%s'%(var,var_fit))
    return var_fit

koopmansfolders=find_files(pwd, header='koopmans', var='',middle='e_', remove=False)
charges=map(extract_charge_comment, koopmansfolders)
charges = set(charges)
assert 0 in charges, 'charge=%s having zero charge folders: %s' % (charges,koopmansfolders)
charges.remove(0) # Now, charges refer to non-zero charges
#charges == {1} or charges == {-1} or charges=={1,-1}


#[1]:
# read the type of the folder from 'DEFECT', will know: interstitial, vacancy, perfect cells
mydictionary=read_incar(pwd, incar='DEFECT')
title_info = mydictionary['DEFECTTYPE']
koopmanstype = mydictionary['KOOPMANSTYPE']
print('This is a %s' % title_info)

#[2]:

#var='HFSCREEN';relax_fix_var='AEXX'
var='AEXX';relax_fix_var='HFSCREEN'

read_fil=read_file_values(pwd)
relax_fix_para=read_fil.incar([relax_fix_var, var])

#[3]:
#read_fil=read_file_values(pwd)
#read_fil.locpot2freysoldt_correction()
read_fil=rotate_read(pwd)
f=lambda x:','.join(x.astype(str))


# Koopmans condition expect E(N+dN)-E(N)=E(N)dN or  E(N+dN) - E(N) - E_ho*dN = 0 
if len(charges)==1:
    dN=-charges.pop() # charges.pop=defect_charge, electron number change = -dN (negativecharge)
    if dN < 0: # remove electron:  #like E(N-1)-E(N)+E_ho=0 where dN=-1 
        ## highest occupied energy: E_ho
        x1,yy1=read_fil.rotate(var,'energyHO', 'koopmans',var,'0e_') # y1=epsilon_ho
        y2legend = 'dN*\epsilon_{ho}(N)'
    elif dN > 0: # add electron: like  E(N+1)-E(N)-E_lu=0 where dN=1
        x1,yy1=read_fil.rotate(var,'energyLU', 'koopmans',var,'0e_') # y1=epsilon_lu
        #yy1=yy1[:,0] # The energy of the highest occupied states # drop the nband in y1 
        y2legend = 'dN*\epsilon_{lu}(N)'
    else:
        print('Error! dN must be nonzero')
        sys.exit()
    yy1=yy1[:,0]*dN #E_ho*dN or E_lu*dN #The energy of the highest occupied states # [:,0] drop the nband in y1 
    # yy3-yy2=the energy difference between positive defect E(N-1) and neutral defect E(N)
    x2,yy2=read_fil.rotate(var,'equil_energy','koopmans', var,'0e_') #Neutral defect LOCPOT, E(N)
    x3,yy3=read_fil.rotate(var,'equil_energy','koopmans', var,'e_',avoid='charge0e_') #Postive defect LOCPOT, E(N-1)
    #  Koopmans' theorem: E(N+dN) - E(N) - E_ho*dN like E(N-1)-E(N)+E_ho (No correction for perfect supercell)
    y1=np.round(yy3-yy2-yy1,6)  #E(N+dN) - E(N) - E_ho*dN
    var_fit = fit_data(var, x1, y1)
    #Later y1=np.round(yy3-yy2,6); y2=np.round(-yy1,6)
    print('\nx=np.array([%s])\n%s=np.array([%s])\nE(N)=np.array([%s])\nE(N%+.2f)=np.array([%s])\n' % ( f(x1),y2legend,f(yy1),f(yy2),dN,f(yy3) ) )
    y1=np.round(yy3-yy2,6) # E(N+dN)-E(N)
    y2=np.round(yy1,6) #\epsilon*dN
    y1legend = 'E(N%+.2f)-E(N)' % (dN) # E(N+dN)-E(N)
    #elif dN > 0 #like E(N+1)-E(N)-E_lu=0 or E(N)-E(N+dN)+E_lu(N)*dN=0 with dN=1
    #    ## lowest unoccupied energy: E_lu
    #    x1,yy1=read_fil.rotate(var,'energyLU', 'koopmans',var,'0e_') # y1=epsilon_lu
    #    yy1=yy1[:,0] # The energy of the lowest unoccupied states # drop the nband in y1 
    #    # yy3-yy2=the energy difference between neutral defect E(N) and negative defect E(N+1)
    #    x2,yy2=read_fil.rotate(var,'equil_energy','koopmans', var,'e_'，avoid='charge0e_') #Negative defect LOCPOT, E(N+1)
    #    x3,yy3=read_fil.rotate(var,'equil_energy','koopmans', var,'0e_') #Neutral defect LOCPOT, E(N)
    #    #  Koopmans' theorem: E(N)-E(N+1)+E_lu=0 or its negative E(N+1)-E(N)-E_lu=0 (No correction for perfect supercell)
    #    y1=np.round(yy3-yy2+yy1,6) #E(N)-E(N+1)+E_lu=0 and dN=1
    #    var_fit = fit_data(var, x1,y1)
    #    print('\nx=np.array([%s])\nE_lu=np.array([%s])\nnegative_energy=np.array([%s])\nneutral_energy=np.array([%s])\n' % ( f(x1),f(yy1),f(yy2),f(yy3) ) )
    #    y1=np.round(yy3-yy2,6) #E(N)-E(N+1)
    #    y2=np.round(-yy1,6) #-E_lu
    #    y1legend = 'E(N)-E(N+1)'

elif charges == {-1,1}: # remove an electron: E(N-1)-2E(N)+E(N+1) = -E_ho(N) + E_lu(N)
    ## lowest unoccupied energy: E_lu
    x1,yy1=read_fil.rotate(var,'energyHO', 'koopmans',var,'0e_') # y1=epsilon_ho
    x1,zz1=read_fil.rotate(var,'energyLU', 'koopmans',var,'0e_') # y1=epsilon_lu
    yy1=yy1[:,0] # The energy of the highest occupied states # drop the nband in y1 
    zz1=zz1[:,0] # The energy of the lowest unoccupied states # drop the nband in y1 
    # yy3-yy2=the energy difference between neutral defect E(N) and negative defect E(N+1)
    x2,yy2=read_fil.rotate(var,'equil_energy','koopmans', var,'charge-1e_') #Negative defect LOCPOT, E(N+1)
    x3,yy3=read_fil.rotate(var,'equil_energy','koopmans', var,'charge0e_') #Neutral defect LOCPOT, E(N)
    x4,yy4=read_fil.rotate(var,'equil_energy','koopmans', var,'charge1e_') #Postive defect LOCPOT, E(N-1)
    #  Koopmans' theorem: E(N-1)-2E(N)+E(N+1) = -E_ho(N) + E_lu(N) (No correction for perfect supercell)
    y1=np.round(yy4 - 2*yy3 + yy2 + yy1 - zz1,6) 
    var_fit = fit_data(var, x1,y1)
    print('\nx=np.array([%s])\nE_ho=np.array([%s])\nE_lu=np.array([%s])\nnegative_energy=np.array([%s])\nneutral_energy=np.array([%s])\npositive_energy=np.array([%s])\n' % ( f(x1),f(yy1),f(zz1),f(yy2),f(yy3),f(yy4) ) )
    y1 = np.round(yy4 - 2*yy3 + yy2,6)
    y2 = np.round(-yy1 + zz1,6)
    y1legend = 'E(N-1)-2E(N)+E(N+1)'
    y2legend = '-\epsilon_{ho}(N)+\epsilon_{lu}(N)'

else:
    print('Error! charge not recognized!')
    sys.exit()

assert np.all(x1==x2) and np.all(x1==x3)





# Ready for plot
x1=x1.astype(str)
y1=y1.astype(str)
y2=y2.astype(str)

#print('Calculate y=Epos-Eneu+E_ho. Expect y=0 satisfy koopmans condition\n')
print("\nx=np.array([%s])\nxlabel=\'%s\'\ny=np.array([%s])\nylabel=\'Koopmans\'\nylabelunit=\' (eV)\'\nlegend=\'$%s$\'\nz=np.array([%s])" % (','.join(x1), var,','.join(y1),y1legend ,','.join(y2)) )

if koopmanstype == 'geometry_fixed':
    title_info = title_info+' fit%s=%.2f' % (var, var_fit)
    print("comment=\'initial_%s=%s_%s=%s\'" % (relax_fix_var, relax_fix_para[0],var,relax_fix_para[1]))
elif koopmanstype == 'geometry_relaxed':
    print("comment=\'unit cell relaxed\'")
else:
    print('Error! koopmanstype not recognized')
print("title=\'%s\'\nplt.plot(x,z,\'o\', label=\'$%s$\')\n" % ( title_info,y2legend ) )

print('\nCopy the above data to draw_temp.koopmans.py, and draw a diagram.')

print('Run with \'python $SCRIPT/draw_temp.koopmans.py\'')
