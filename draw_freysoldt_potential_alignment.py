import numpy as np
import sys 
import shutil
import os
import matplotlib.pyplot as plt 
sys.path.append(os.environ['SCRIPT'])
path = os.environ['PWD'] + '/' 
from class0_functions1 import read_incar

# No need to call this function directly. So far this is embedded in 'inter_defect_update_freysoldtcorr.sh'
# Need to start with the folder storing vline-eV-a0.dat
# this file is quite similar to 'out_koopmans3_freysoldt_para_C.py'

def farthest_pos(defectcenter, coor):
        xx=defectcenter[coor]
        xa=xx-0.5
        xb=xx+0.5
        if xa>=0 and xa <1:
                awaycenter=xa
        elif xb>=0 and xb <1:
                awaycenter=xb
        return np.round(awaycenter,6)

def get_C_for_freysoldt(fil, defectcenter):
        '''
        Enter 
        '''
        coor=int(fil[-5]) #'vline-eV-a0.dat'[-5] --> read '0'

        ##Vo: defectcenter=[0.50,0.42,0.56]
        #awaycenter=np.array([0.,0.42,0.56])[coor]
        awaycenter = farthest_pos(defectcenter, coor) # the frac_coord of position farthest away from defect
        print('x%s fractional coor: the defect is %s the furthest is %s' % (coor, defectcenter[coor], awaycenter))

        # x1 y1 gives the real potential
        x1=np.array([]) 
        y1=np.array([])
        # x2 y2 gives the predicted potential
        x2=np.array([])
        y2=np.array([])
        # x2 y3 gives the difference. Read potential alignment from here
        y3=np.array([])
        with open(fil, 'r') as f:
                lines=f.readlines()
                totlen=len(lines)
                segment=int((totlen-1)/2)
                #assert type(segment)==int, 'The position of & should be an integer'
                for i in range(segment):
                        x,y=lines[i].split()
                        x1=np.append(x1,float(x))
                        y1=np.append(y1,float(y)) # the model, will be a smooth function
                for i in range(segment+1,totlen):
                        #print(path+fil, totlen,i,lines[i])
                        x,y,z=lines[i].split()
                        x2=np.append(x2,float(x))
                        y2=np.append(y2,float(y)) # delta V, Pot(defect)-Pot(bulk)
                        y3=np.append(y3,float(z)) #  alignment term
        assert np.all(x1==x2)
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
        plt.plot(x1,y1,'k',label='$V^{lr}_q$')
        plt.plot(x1,y2,'r:',label='$\Delta V=V_{defect}-V_{bulk}$')
        plt.plot(x1,y3,'g--',label='$V^{sr}_q=\Delta V-V^{lr}_q$')
        plt.legend()
        plt.xlabel(['x(bohr)','y(bohr)','z(bohr)'][coor])
        plt.ylabel('potential (V)')

        x_awaycenter=x1[-1]*awaycenter # awaycenter is the fractional cooedinate, get the cart_coord 'x_awaycenter'
        def closest(lst, K): # list, the value
                key=np.argmin(np.abs(lst-K)) # find the key to make x1closed to awaycenter
                print('neighbor of K=%s is %s'% (K, lst[key-2:key+2]))
                return key
        key=closest(x1,x_awaycenter) # find x1 that is closed to awaycenter and
        cc = np.round(y3[key],4) # parameter C
        print('The file to open is %s, the plot is about position a%s and C=%s' %(fil, coor, cc) )
        plt.title('Electrostatic potentials $x_{far}$=%.3f C=%.3f' % (x_awaycenter, cc))
        plt.tight_layout()
        plt.savefig('pyplt_BeO_%s%se_fc_%s.pdf'% ( sys.argv[1],sys.argv[2], fil ), dpi=600)
        # sys.argv[1] is defecttype
        # sys.argv[2] is charge 
        plt.close()
        #print('x1=%s\nx2=%s\n'%(x1,x2))
        #print('y1=%s\ny2=%s\ny3=%s\n'%(y1,y2,y3))
        return cc


if len(sys.argv)<4:
        defectcenter = read_incar(path, incar='SAVEINFO')['CENTER']  # position of defect
else:
        defectcenter = sys.argv[3]
#defectcenter=read_incar(path, incar='SAVEINFO')['CENTER']  # position of defect
print('defectcenter is at %s' % defectcenter)
defectcenter=np.array(defectcenter.split(',')).astype(float)

fil='vline-eV-a0.dat' 
para_C0=get_C_for_freysoldt(fil,defectcenter)
fil='vline-eV-a1.dat' 
para_C1=get_C_for_freysoldt(fil,defectcenter)
fil='vline-eV-a2.dat' 
para_C2=get_C_for_freysoldt(fil,defectcenter)
#print('para_C0=%s para_C1=%s para_C2=%s' % (para_C0, para_C1, para_C2))

para_C = np.round(np.mean([para_C0, para_C1, para_C2]),4) # average para_C in three directions
print('paraC=%s \n'% para_C)

#with open('freysoldt_paraC_values.py', 'w') as f:
#        f.write('import numpy as np\n')
#        f.write('para_C=np.array(%s).reshape((-1,3)) \n'%(para_C))
#        #f.write('para_C=para_C.reshape((-1,3))\n')
