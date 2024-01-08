import numpy as np
import sys 
import shutil
import os
path = os.environ['PWD'] + '/' 
sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import read_incar, find_files
# Need to start with the folder storing vline-eV-a0.dat
# this file is quite similar to 'draw_freysoldt_potential_alignment.py'
#fil=sys.argv[1]
#ref='bulk'
ref='defect'
f1='sync_freysoldt_ref-'+ref # folder of file 1
path = path+f1
os.chdir(path)


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
        coor=int(fil[-5])
        print('The file to open is %s, the plot is about position x%s' %(fil,coor) )

        #print(defectcenter)
        awaycenter=farthest_pos(defectcenter, coor) # the frac_coord of position farthest away from defect #np.array([0.,0.42,0.40])[coor]
        print('x%s fractional coor: the defect is %s the furthest is %s' % (coor, defectcenter[coor], awaycenter))

        x1=np.array([])
        y1=np.array([])
        x2=np.array([])
        y2=np.array([])
        y3=np.array([])

        with open(fil, 'r') as f:
                lines=f.readlines()
                totlen=len(lines)
                segment=int((totlen-1)/2)
                #assert type(segment)==int, 'The position of & should be an integer'
                for i in range(segment):
                        x,y=lines[i].split()
                        x1=np.append(x1,float(x))
                        y1=np.append(y1,float(y))
                for i in range(segment+1,totlen):
                        x,y,z=lines[i].split()
                        x2=np.append(x2,float(x))
                        y2=np.append(y2,float(y))
                        y3=np.append(y3,float(z))
        assert np.all(x1==x2)

        x_awaycenter=x1[-1]*awaycenter
        def closest(lst, K): # find index corresponds to the x that is close to awaycenter K
                key=np.argmin(np.abs(lst-K)) # lst is coordinate and it increases linearly. key would be unique
                #print('neighbor of K=%s is %s'% (K, lst[key-2:key+2]))
                return key
                #return lst[min(range(len(lst)), key = lambda i: abs(lst[i]-K))] 
        key=closest(x1,x_awaycenter) # find index corresponds to the x that is close to awaycenter
        print('C=%s\n'% y3[key])
        return y3[key]
        #print('x1=%s\nx2=%s\n'%(x1,x2))
        #print('y1=%s\ny2=%s\ny3=%s\n'%(y1,y2,y3))
defectcenter=read_incar(os.environ['PWD'], incar='DEFECT')['CENTER']
defectcenter=np.array(defectcenter.split(',')).astype(float)

existed_files=find_files(path,header='',var='.dat',remove=False)
para_C=[]
for fil in existed_files:
        para_C.append(get_C_for_freysoldt(fil, defectcenter))
print(para_C)

with open('freysoldt_C_values.py', 'w') as f:
        f.write('import numpy as np\n')
        f.write('para_C=np.array(%s).reshape((-1,3)) \n'%(para_C))
        #f.write('para_C=para_C.reshape((-1,3))\n')

