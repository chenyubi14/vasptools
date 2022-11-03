import numpy as np
import os
import sys


def read_interpolate_2D_data(x1,x2,y,findx1,findx2):
    # x1 and x2 are coming as either a meshgrid or unique items
    # (findx1, findx2) is the data point to find y
    # y is either bandgap, dielectric constant 
    # x1 should look like array([[0.  , 0.05, 0.1 ],[0.  , 0.05, 0.1 ]])
    # x2 should look like array([[0.32, 0.32, 0.32], [0.34, 0.34, 0.34]])
    # y(ind2,ind1): x1 will take the index on the right (smallest change), x2 will take the left one
    x1=np.unique(x1)
    x2=np.unique(x2)
    findx1=float(findx1)
    findx2=float(findx2)
    assert y.shape == (len(x2),len(x1))
    # np.where(x1==findx1) returns (array([1]),), so [0][0] will give the value inside
    ind1=np.where(x1==findx1)[0][0]
    ind2=np.where(x2==findx2)[0][0]
    findy=y[ind2,ind1]
    return findy

def get_2D_electronic_eps(dict_find):
    # find the value 'find1' of 'name1' e.g. the value find1=0.32 of 'AEXX'
    # input a dictionary of values {name1:find1, name2:find2}
    sys.path.append(os.environ['WORK'])
    from ELECTRONIC_EPS import x,y,xname
    # need to change this
    xname=xname.split('_')
    x1=x[0]
    x2=x[1]
    findy=read_interpolate_2D_data(x1,x2,y,dict_find[xname[0]], dict_find[xname[1]])
    return findy

def write_DEFECT(direc, dictionary={}, incarname='DEFECT'): 
    # keywords are self-defined
    # DEFECT stores the information of defect such as 
    # CENTER=position of defect center
    # NLINE= the vim line to find defect (or its neighbors)
    # FREYCORR = freysoldt correction
    # EPS= dielectric constant
    # EPS_ELEC = electronic dielectric constant 
	if direc[-1]!='/':
		direc+='/'
	with open(direc+incarname, 'w') as f:
		for key in dictionary.keys():
			f.write('%s%s=%s \n' % ( '', key, str(dictionary[key])) )

def read_interpolate_1D_data(x1,x2,y,findx1,findx2):
    # x1,x2,y are 1D data. The same index correspond to the same calculation.
    # E.g. x1(aexx)=[0,1,0,1], x2(hfscreen)=[0.1,0.1,0.2,0.2], y(bandgap)=related values
    # find the index when x1=findx1 and x2=findx2
    findind=np.where((x1==findx1)*(x2==findx2))[0] # np.where gives (array([9]),) 
    assert len(findind)==1 ,'must be one found value' # findind will be array([9])
    findy=y[findind[0]] # use findind[0]=9 to give the index number 
    return findy

def reorder_x_y_data(x,y,data):
    # x,y,data should come as the 1-d arrays
    # make x,y a meshgrid
    # make data the correct format
    uniquex=np.unique(x) # unique will sort the values automatically
    uniquey=np.unique(y)
    # generate a meshgrid
    xx,yy=np.meshgrid(uniquex,uniquey) # shape is (len(uniquey),len(uniquex))
    data_xx_yy=np.zeros(xx.shape)
    for i in range(len(uniquey)):
        for j in range(len(uniquex)):
            #read_interpolate_1D_data(x1,x2,y,findx1,findx2)
            data_xx_yy[i,j]= read_interpolate_1D_data(x,y,data,xx[i,j],yy[i,j]) 
            # get data[i,j] to match with meshgrid xx,yy
    return xx,yy,data_xx_yy

#def calc_deltamiu(enthalpyf_ele, ele, dict_elementformula): # formation enthalpy = num_Be * deltamiu_Be + num_O * deltamiu_O 
#    num_ele = dict_elementformula[ele] # number of element atoms
#    return enthalpyf_ele / num_ele 


def generate_KPATH(kpathfolder=''):
    '''
    kpathfolder: the folder of KPATH
    generate KPOINTS from KPATH
    '''
    pwd = os.getcwd() + '/'
    if kpathfolder!='':
        print('use KPATH from folder %s to generate KPOINTS' % kpathfolder)
    else:
        hier1direc = parent_folder(pwd, hier=10)[0] # 1st-class material folder
        kpathfolder = hier1direc + 'sources/'

    # file KPATH specifies band information
    with open(kpathfolder+'KPATH','r') as f1: 
        lines = f1.readlines()
        N = int(lines[0].split()[2]) # split a kpoint path gamma->A into N segments
        Vec = []
        KPOINTS = np.array([])
        for i in range(1,len(lines)):
            splitted = np.array(lines[i].split())
            Vec.append(splitted[0]) # ['G', 'M', 'K', 'G', 'A', 'L', 'H', 'A']
            KPOINTS = np.append(KPOINTS, splitted[1:].astype(float)) # have to read as numbers
        KPOINTS = np.reshape(KPOINTS,(-1,3))
        # make comments for the special KPOINTS, no comment for the interpolated points
        commentkp=[Vec[0]] # ['G']
        for i in range(1,len(Vec)):
            comment_i = ['']*(N-1) # ['','',...,'']
            comment_i.append(Vec[i]) # ['','',...,'','M']
            comment_i.append(Vec[i]) # ['','',...,'','M','M']
            commentkp=commentkp+comment_i # ['G', '','',..., '','M','M']
        commentkp = commentkp[:-1]
        print('comment length',len(commentkp))
    #kpath = np.array([])
    kpath = np.linspace(KPOINTS[0], KPOINTS[1], N+1) # length of kpath = N+1, there are N-1 mid points 
    for i in range(1, len(Vec)-1): # stack different paths together
        kpath = np.vstack((kpath, np.linspace(KPOINTS[i], KPOINTS[i+1], N+1))) # length of kpath = N+1, there are N-1 mid points 
        # need to double the intermediate points. No special attention to take like the below
    #kpath = np.linspace(KPOINTS[0], KPOINTS[1], N+1) # length of kpath = N+1, there are N-1 mid points 
    #for i in range(1, len(Vec)-1): # stack different paths together
    #kpath = np.vstack((kpath, np.linspace(KPOINTS[i], KPOINTS[i+1], N+1)[1:] )) # [1:] omits the same points, but now don't omit
    nkpts = len(kpath)

    with open(pwd+'IBZKPT','r') as f2: # read BZ kpoints
        lines = f2.readlines()
        nkibz = int(lines[1])
        nktot = nkibz + nkpts

    # Write KPOINTS with new kpoints
    with open(pwd+'KPOINTS','w') as f3:
        f3.write("Automatically generated mesh following KPATH\n%s\nReciprocal lattice\n" % (nktot))
        write_content = ''.join(lines[3: nkibz+3]) # join the list to a string
        f3.write(write_content)
        # add zero share to each kpoint
        kpath = np.round(kpath,8)
        kpath = np.c_[kpath, np.zeros(len(kpath))]
        kpath = np.c_[kpath, commentkp]
        for i in range(nkpts):
            f3.write("%s %s %s %s\t\t%s\n" % tuple(kpath[i]))


