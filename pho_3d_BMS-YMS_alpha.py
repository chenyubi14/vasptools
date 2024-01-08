# In[1]:
import numpy as np
from pymatgen.core import Structure

# Goal: count neighbors along distances


# In[3]:
poscar='POSCAR'
structpos=Structure.from_file(poscar)
sb1=structpos[11]
sb2=structpos[10]

p1=sb1.coords
p2=sb2.coords
sb1.a=1.0
p3=sb1.coords

p1=np.round(p1,7)
p2=np.round(p2,7)
p3=np.round(p3,7)
#print('Three points:', p1, p2, p3)
v1 = p1 - p2
v2 = p3 - p2

angle = np.arccos( np.dot(v1,v2) / (np.linalg.norm(v1)*np.linalg.norm(v2)) )
#angle = np.arccos(angle)

print('Angle is %s = %.2f degrees\n' % (angle,angle/np.pi*180)) 







