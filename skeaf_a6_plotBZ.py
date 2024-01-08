#!/usr/bin/env python

import numpy as np
from ase.io import read
import os
import sys
import argparse

############################################################
## Edit here
bzpoints1=np.array([
    [0,0,0],
    [1/2,0,0],
    [1/3,1/3,0],
    [0,0,0],
    [0,0,1/2],
    [1/2,0,1/2],
    [1/3,1/3,1/2],
    [0,0,1/2],
    [1/2,0,1/2],
    [1/2,0,0],
    [1/3,1/3,0],
    [1/3,1/3,1/2],
])
shift1=2*np.array([
    [0,-0.0,0],
    [0,-0.0,0],
    [0,0,0],
    [0,-0.0,0],
    [0.0,0.0,0],
    [0,0,0],
    [0,0,0],
    [0,0,0],
    [0,0,0],
    [0,0,0],
    [0,0,0],
    [0,0,0],
    [0,0,0],
])
bzpoints1_name = ['$\Gamma$','M','K','\Gamma','A','L','H','A','L','M','K','H']
#bzpoints2=np.array([
#    [0,0,1/2],
#    [0,1/2,1/2],
#    [1/2,1/2,1/2],
#    [0,0,1/2],
#    [1/2,0,1/2],
#    [1/2,1/2,1/2]
#])
#shift2=2*np.array([
#    [0,-0.01,-0.001],
#    [0.01,-0.01,0],
#    [0.02,0.01,0],
#    [0,0,0],
#    [0.04,0.025,0],
#    [0,0,0]
#])
#bzpoints2_name = ['Z','T','R','Z','U','R']
############################################################


print('2pi is NOT in k definition!')

def get_brillouin_zone_3d(cell):
    """
    Generate the Brillouin Zone of a given cell. The BZ is the Wigner-Seitz cell
    of the reciprocal lattice, which can be constructed by Voronoi decomposition
    to the reciprocal lattice.  A Voronoi diagram is a subdivision of the space
    into the nearest neighborhoods of a given set of points. 

    https://en.wikipedia.org/wiki/Wigner%E2%80%93Seitz_cell
    https://docs.scipy.org/doc/scipy/reference/tutorial/spatial.html#voronoi-diagrams
    """

    cell = np.asarray(cell, dtype=float)
    assert cell.shape == (3, 3)

    px, py, pz = np.tensordot(cell, np.mgrid[-1:2, -1:2, -1:2], axes=[0, 0])
    points = np.c_[px.ravel(), py.ravel(), pz.ravel()]

    from scipy.spatial import Voronoi
    vor = Voronoi(points)

    bz_facets = []
    bz_ridges = []
    bz_vertices = []

    for pid, rid in zip(vor.ridge_points, vor.ridge_vertices):
        # WHY 13 ????
        # The Voronoi ridges/facets are perpendicular to the lines drawn between the
        # input points. The 14th input point is [0, 0, 0].
        if(pid[0] == 13 or pid[1] == 13):
            bz_ridges.append(vor.vertices[np.r_[rid, [rid[0]]]])
            bz_facets.append(vor.vertices[rid])
            bz_vertices += rid

    bz_vertices = list(set(bz_vertices))

    return vor.vertices[bz_vertices], bz_ridges, bz_facets


def get_primitive_cell_3d(cell):
    """
    Get the vertices, lines and facets of the primitive cell.
    """
    cell = np.asarray(cell, dtype=float)
    assert cell.shape == (3, 3)

    dx, dy, dz = np.mgrid[0:2, 0:2, 0:2]
    dxyz = np.c_[dx.ravel(), dy.ravel(), dz.ravel()]
    px, py, pz = np.tensordot(cell, [dx, dy, dz], axes=[0, 0])
    points = np.c_[px.ravel(), py.ravel(), pz.ravel()]

    lines = []
    faces = None

    for ii in range(len(points)):
        for jj in range(ii):
            if np.abs(dxyz[ii] - dxyz[jj]).sum() == 1:
                lines.append(np.vstack([points[ii], points[jj]]))

    return points, lines, faces


class ebands3d(object):

    def __init__(self, bxsf, efermi=None):
        '''
        Initialization
        '''
        self._fname = bxsf
        # the directory containing the input file
        self._dname = os.path.dirname(self._fname)
        if self._dname == '':
            self._dname = '.'
        # read bands, k-points of the irreducible Brillouin Zone
        self.read_bxsf() # self.efermi is read from .bxsf 
        # reset the Fermi energy by the given value
        if efermi:
            self.efermi=efermi


    def show_fermi_surf(self, cell='bz', fbcell=None,
                      cmap='Spectral'):
        '''
        Plotting the Fermi surface within the BZ using matplotlib.
        '''
        #savefig='fs_bxsf_%.3f.pdf' % self.efermi
        #savefig='BZ_kpath.png' 
        savefig='BZ_kpath.pdf' 

        try:
            # from skimage.measure import marching_cubes_lewiner as marching_cubes
            from skimage.measure import marching_cubes as marching_cubes
        except ImportError:
            try:
                from skimage.measure import marching_cubes
            except ImportError:
                raise ImportError("scikit-image not installed.\n"
                                  "Please install with it with `conda install scikit-image` or `pip install scikit-image`")

        bcell = self.reclat # bcell is reciprocal lattice vector
        b1, b2, b3 = np.linalg.norm(bcell, axis=1)

        if cell == 'bz':
            # the vertices, rigdges and facets of the BZ
            p, l, f = get_brillouin_zone_3d(bcell)

            from scipy.spatial import cKDTree
            px, py, pz = np.tensordot(
                self.reclat, # reciprocal lattice vector
                np.mgrid[-1:2, -1:2, -1:2], # -1:2 means -1,0,1 # is a 3*3*3 kpoint
                axes=[0, 0]
            )
            points = np.c_[px.ravel(), py.ravel(), pz.ravel()]
            tree = cKDTree(points)
            ## Gamma point belong to the first BZ.
            gamma_region_id = tree.query([0, 0, 0])[1]
        else:
            # the vertices, rigdges and facets of the primitive cell
            p, l, f = get_primitive_cell_3d(bcell)


        ############################################################
        # Plot the Fermi surface using matplotlib
        ############################################################
        import matplotlib as mpl
        import matplotlib.pyplot as plt
        from mpl_toolkits.mplot3d import Axes3D
        from mpl_toolkits.mplot3d.art3d import Poly3DCollection


        fig = plt.figure(figsize=(6, 6))
        ax = fig.add_subplot(111, projection='3d')
        # ax.set_aspect('equal')
        ############################################################

        basis_vector_clrs = ['orange', 'g', 'b']
        basis_vector_labs = ['$x$', '$y$', '$z$']
        ############################################################
        ### ratios for xyz axis
        ratios = [1.5,1.5,0.4]
        ## shift vector for label xyz
        shift_xyz =np.array([ [0,0,0], [0,0,0], [0,0,0] ])
        ############################################################
        if fbcell == None:
            cell_use = bcell
        else:
            fbcell = np.loadtxt(fbcell)
            assert fbcell.shape == (3,3), 'the rec_latt stored in fbcell has a wrong format'
            cell_use = fbcell
        for ii in range(3):
            max_b = np.max([b1,b2,b3]) * 0.8 
            bi = cell_use[ii]
            bi = bi/np.linalg.norm(bi) * max_b * ratios[ii]
            #ax.quiver(0,0,0, *bi ,color=basis_vector_clrs[ii])
            ax.plot([0, bi[0] ], [0, bi[1] ], [0, bi[2]],
                    color=basis_vector_clrs[ii], lw=1.5,linestyle='dashed')
            ax.text(bi[0]+shift_xyz[ii,0], 
                    bi[1]+shift_xyz[ii,1], 
                    bi[2]+shift_xyz[ii,2],
                    basis_vector_labs[ii], fontsize=24)
        ############################################################
        # Plot the Brillouin Zone
        ############################################################
        # The BZ outlines
        for xx in l:
            ax.plot(xx[:, 0], xx[:, 1], xx[:, 2], color='k', lw=1.0)
        ############################################################
        #if cell == 'bz':
        #    ax.set_xlim(-b1, b1)
        #    ax.set_ylim(-b2, b2)
        #    ax.set_zlim(-b3, b3)
        if cell == 'bz':
            max_b = np.max([b1,b2,b3]) * 0.5
            ax.set_xlim(-max_b,max_b)
            ax.set_ylim(-max_b,max_b)
            ax.set_zlim(-max_b,max_b)
        else:
            ax.set_xlim(0, b1)
            ax.set_ylim(0, b2)
            ax.set_zlim(0, b3)
        ############################################################
        ## edit the view angle
        #ax.view_init(20, 30)
        ax.set_box_aspect([1,1,1])
        ############################################################

         
        # change k-points in fractional coordinates to real coordinates
        def frac2real(bzpoints):
            vectors=[] 
            for point in bzpoints: 
                vector=point[0]*bcell[0]+point[1]*bcell[1]+point[2]*bcell[2]
                vectors.append(list(vector)) 
            return np.array(vectors)  
        global bzpoints1
        bzpoints1=frac2real(bzpoints1) 
        #global bzpoints2
        #bzpoints2=frac2real(bzpoints2) 


        ax.plot(bzpoints1[:,0], bzpoints1[:,1], bzpoints1[:,2],
                color='purple', lw=2) #, linestyle='dashed')
        ax.scatter(bzpoints1[:,0], bzpoints1[:,1], bzpoints1[:,2],marker='o',
                color='red', lw=2) #, linestyle='dashed')
        #ax.plot(bzpoints2[:,0], bzpoints2[:,1], bzpoints2[:,2],
        #        color='purple', lw=2) #, linestyle='dashed')
        #ax.scatter(bzpoints2[:,0], bzpoints2[:,1], bzpoints2[:,2],marker='o',
        #        color='red', lw=2) #, linestyle='dashed')
        def add_bz_label(bzpoints,names,shift=np.zeros((10,3)) ):
            _,indices=np.unique(bzpoints,return_index=True,axis=0)
            for i in indices: #zip(bzpoints[indices],names[indices]):
                point=bzpoints[i] + shift[i]
                name=names[i]
                ax.text(point[0], point[1], point[2],name,fontsize=18)
        add_bz_label(bzpoints1,bzpoints1_name,shift1)
        #add_bz_label(bzpoints2,bzpoints2_name,shift2)

        # Hide grid lines
        ax.grid(False)
        plt.axis('off')
        plt.grid(b=None)
        
        # Hide axes ticks
        ax.set_xticks([])
        ax.set_yticks([])
        ax.set_zticks([])


        # plt.tight_layout()
        plt.savefig(savefig, dpi=600, transparent=True)
        print('figure %s is generated!' % (savefig) )
        plt.show()
        plt.close()
        ############################################################


    def read_bxsf(self):
        '''
        Read the reciprocal lattice from bxsf file
        '''
        from skeaf_a2_bxsf_parser import BxsfParser
        dat = BxsfParser(infile=self._fname)
        self.reclat = dat.reclat 	# reciprocal lattice vector




def parse_cml_args(cml):
    '''
    CML parser.
    '''
    arg = argparse.ArgumentParser(add_help=True)

    #arg.add_argument('-i', dest='bxsf', action='store', type=str,
    arg.add_argument('bxsf', action='store', type=str,
                     help='Please enter the bxsf file')
    arg.add_argument('--cell', dest='cell', action='store', type=str,
                     default='bz', choices=['uc', 'bz'],
                     help='Show Fermi surface in BZ or primitive unit cell?')
    arg.add_argument('--efermi', dest='efermi', action='store', type=float,
                     default=None,
                     help='the Fermi energy. By default, will use bxsf value')
    arg.add_argument('--fbcell', dest='fbcell', action='store', type=str,
                     default=None,
                     help='the filename of reciprocal lattice used in plotting. You may want to change rec_latt to other directions like unit cell. By default it will use rec_latt in bxsf file')

    return arg.parse_args(cml)


def main(cml): # cml: command line labels
    p = parse_cml_args(cml)

    fs = ebands3d(bxsf=p.bxsf, efermi=p.efermi)
    fs.show_fermi_surf(cell=p.cell, fbcell=p.fbcell)


if __name__ == "__main__":
    main(sys.argv[1:])
