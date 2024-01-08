#!/usr/bin/env python

import numpy as np
from ase.io import read
import os
import sys
import argparse
import matplotlib.pyplot as plt 

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

    # for rid in vor.ridge_vertices:
    #     if( np.all(np.array(rid) >= 0) ):
    #         bz_ridges.append(vor.vertices[np.r_[rid, [rid[0]]]])
    #         bz_facets.append(vor.vertices[rid])

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
    '''
    '''

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
        self.find_fermicrossing_bands()
        # set the k-points mesh
        #self.set_kmesh(kmesh) # note the kmesh is 12*12*12 in KPOINTS, but 13*13*13 in bxsf file
        # read POSCAR
        #self.atoms = read(self.poscar)
        # create the grid to ir map
        #self.ir_kpts_map(symprec=symprec)
        # 
        self.get_fermi_ebands3d()



    def get_fermi_ebands3d(self):
        '''
        For those bands that cross the Fermi level, unfold the band energies on
        the irreducible BZ onto the whole reciprocal primitive cell.
        '''

        # band energies of the k-points within the primitive cell
        self.fermi_ebands3d_uc = []
        # band energies of the k-points within the Brillouin Zone
        # self.fermi_ebands3d_bz = []

        # nx, ny, nz = self.kmesh
        for ispin in range(self.nspin):
            uc_tmp = []
            # bz_tmp = []
            for iband in self.fermi_xbands[ispin]: # the band that crosses with Fermi level
                # the band energies of the k-points within primitive cell
                #etmp = self.ir_ebands[ispin, self.grid_to_ir_map, iband]
                etmp = self.ir_ebands[ispin, :, iband] # shape is (nx*ny*nz)
                etmp.shape = list(self.kmesh) # shape is (nx, ny, nz)
                # I removed 'reversed' in the original form: list(reversed(self.kmesh))
                # it is because, in the original form, x-index in the "grid" runs fastest # which is given by self.fs_kpath
                # # make the band energies periodic in the primitive cell
                # etmp = np.tile(etmp, (2,2,2))[:nx+1, :ny+1, :nz+1]
                uc_tmp.append(etmp)

                # # the band energies of the k-points within Brillouin Zone
                # btmp = np.tile(etmp, (2,2,2))
                # s = btmp.shape
                # btmp.shape = (btmp.size)
                # # set the band energies of the k-points outside BZ to a large
                # # one so that the energy isosurface will not extent outside
                # # beyond the BZ.
                # btmp[np.logical_not(self.bz_in_kgrid_2uc)] = self.emax + 100.
                # btmp.shape = s
                # bz_tmp.append(btmp)

            self.fermi_ebands3d_uc.append(uc_tmp)
            # self.fermi_ebands3d_bz.append(bz_tmp)

        # periodic band energies, mesh size +1
        # self.kmesh = [nx+1, ny+1, nz+1]


    def show_fermi_surf(self, cell='bz', fbcell=None,
                      cmap='Spectral'):
        '''
        Plotting the Fermi surface within the BZ using matplotlib.
        '''
        savefig='fs_bxsf_%.3f.png' % self.efermi

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

            #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # https://docs.scipy.org/doc/scipy/reference/tutorial/spatial.html#voronoi-diagrams
            # https://docs.scipy.org/doc/scipy/reference/spatial.html#voronoi-diagrams
            # cKDTree is implemented in cython, which is MUCH MUCH FASTER than KDTree
            #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            from scipy.spatial import cKDTree
            px, py, pz = np.tensordot(
                self.reclat, # reciprocal lattice vector
                np.mgrid[-1:2, -1:2, -1:2], # -1:2 means -1,0,1 # is a 3*3*3 kpoint
                axes=[0, 0]
            )
            points = np.c_[px.ravel(), py.ravel(), pz.ravel()]
            tree = cKDTree(points)
            #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # Gamma point belong to the first BZ.
            #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

        ############################################################

        fig = plt.figure(figsize=(6, 6))
        ax = fig.add_subplot(111, projection='3d')
        # ax.set_aspect('equal')
        ############################################################

        basis_vector_clrs = ['r', 'g', 'b']
        basis_vector_labs = ['x', 'y', 'z']
        if fbcell == None:
            #for ii in range(3):
            #    ax.plot([0, bcell[ii, 0]], [0, bcell[ii, 1]], [0, bcell[ii, 2]],
            #            color=basis_vector_clrs[ii], lw=1.5)
            #    ax.text(bcell[ii, 0], bcell[ii, 1], bcell[ii, 2],
            #            basis_vector_labs[ii])
            for ii in range(3):
                max_b = np.max([b1,b2,b3]) * 0.6 
                bi = bcell[ii]
                bi = bi/np.linalg.norm(bi) * max_b
                ax.plot([0, bi[0] ], [0, bi[1] ], [0, bi[2]],
                        color=basis_vector_clrs[ii], lw=1.5)
                ax.text(bi[0], bi[1], bi[2],
                        basis_vector_labs[ii])
        else:
            fbcell = np.loadtxt(fbcell)
            assert fbcell.shape == (3,3), 'the rec_latt stored in fbcell has a wrong format'
            for ii in range(3):
                max_b = np.max([b1,b2,b3]) * 0.6 
                bi = fbcell[ii]
                bi = bi/np.linalg.norm(bi) * max_b
                ax.plot([0, bi[0] ], [0, bi[1] ], [0, bi[2]],
                        color=basis_vector_clrs[ii], lw=1.5)
                ax.text(bi[0], bi[1], bi[2],
                        basis_vector_labs[ii])
            #for ii in range(3):
            #    ax.plot([0, b1*fbcell[ii, 0]], [0, b2*fbcell[ii, 1]], [0, b3*fbcell[ii, 2]],
            #            color=basis_vector_clrs[ii], lw=1.5)
            #    ax.text(b1*fbcell[ii, 0], b2*fbcell[ii, 1], b3*fbcell[ii, 2],
            #            basis_vector_labs[ii])
        ############################################################
        # Plot the Fermi Surface.
        # Marching-cubes algorithm is used to find out the isosurface.
        ############################################################
        for ispin in range(self.nspin):
            for ii in range(len(self.fermi_xbands[ispin])):
                # the band energies in the uc [0, 1]
                b3d = self.fermi_ebands3d_uc[ispin][ii]
                if cell == 'bz':
                    # expand the band energies to double uc, [-1, 1]
                    b3d_2uc = np.tile(b3d, (2, 2, 2))
                    nx, ny, nz = b3d_2uc.shape

                    # https://scikit-image.org/docs/stable/api/skimage.measure.html#skimage.measure.marching_cubes_lewiner
                    verts, faces, normals, values = marching_cubes(b3d_2uc,
                                                                   level=self.efermi,
                                                                   spacing=(
                                                                       2*b1/nx, 2*b2/ny, 2*b3/nz)
                                                                   )
                    verts_cart = np.dot(
                        verts / np.array([b1, b2, b3]) - np.ones(3),
                        bcell
                    )
                    # the region id of the vertices
                    verts_region_id = tree.query(verts_cart)[1]
                    # whether the k-points are in BZ?
                    verts_in_bz = (verts_region_id == gamma_region_id)
                    # find out the triangles with all vertices inside BZ
                    verts_cart_fs = verts_cart[faces][
                        np.alltrue(verts_in_bz[faces], axis=1)
                    ]
                else:
                    nx, ny, nz = b3d.shape
                    # make band energies periodic in primitive cell
                    # b3d = np.tile(b3d, (2,2,2))[:nx+1, :ny+1, :nz+1]
                    b3d = np.pad(b3d, (0,1), mode='wrap') # mayby a little faster?

                    # https://scikit-image.org/docs/stable/api/skimage.measure.html#skimage.measure.marching_cubes_lewiner
                    verts, faces, normals, values = marching_cubes(b3d,
                                                                   level=self.efermi,
                                                                   spacing=(
                                                                       b1/nx, b2/ny, b3/nz)
                                                                   )
                    verts_cart_fs = np.dot(
                        verts / np.array([b1, b2, b3]),
                        bcell
                    )[faces]

                cc = np.linalg.norm(np.sum(verts_cart_fs, axis=1), axis=1)
                nn = mpl.colors.Normalize(vmin=cc.min(), vmax=cc.max())

                art = Poly3DCollection(verts_cart_fs, facecolor='r',
                                       alpha=0.8, color=mpl.cm.get_cmap(cmap)(nn(cc)))
                # art.set_edgecolor('k')
                ax.add_collection3d(art)

        ############################################################
        # Plot the Brillouin Zone
        ############################################################

        # The BZ outlines
        for xx in l:
            ax.plot(xx[:, 0], xx[:, 1], xx[:, 2], color='k', lw=1.0)
        # art = Poly3DCollection(f, facecolor='k', alpha=0.1)
        # ax.add_collection3d(art)
        ############################################################
        #if cell == 'bz':
        #    ax.set_xlim(-b1, b1)
        #    ax.set_ylim(-b2, b2)
        #    ax.set_zlim(-b3, b3)
        if cell == 'bz':
            max_b = np.max([b1,b2,b3]) * 0.6 
            ax.set_xlim(-max_b,max_b)
            ax.set_ylim(-max_b,max_b)
            ax.set_zlim(-max_b,max_b)
        else:
            ax.set_xlim(0, b1)
            ax.set_ylim(0, b2)
            ax.set_zlim(0, b3)
        ax.set_box_aspect([1,1,1])

        # plot a sphere for Cd3As2 HERE!!!
        if False:
            boundary_Z=0.2455/np.pi/2
            z0 = boundary_Z*2/12.5
            r = boundary_Z/12.5
            u, v = np.mgrid[0:2*np.pi:5j, 0:np.pi:5j]
            x = r* np.cos(u)*np.sin(v)
            y = r* np.sin(u)*np.sin(v)
            z = z0 + r* np.cos(v)
            ax.plot_wireframe(x, y, z, color="r",alpha=0.3)

        #ax.set_title('Fermi Energy: {:.4f} eV'.format(self.efermi),
        #             fontsize='small')

        # plt.tight_layout()
        ax.set_axis_off()
        plt.savefig(savefig, dpi=600, transparent=True)
        print('figure %s is generated!' % (savefig) )
        plt.show()
        ############################################################





    def find_fermicrossing_bands(self):
        '''
        Find the index of the bands that cross the Fermi level.
        '''

        band_index = np.arange(self.nbnds, dtype=int)

        band_energy_max = np.max(self.ir_ebands, axis=1)
        band_energy_min = np.min(self.ir_ebands, axis=1)
        fermi_cross_band = (band_energy_min < self.efermi) & (
            self.efermi < band_energy_max)

        self.fermi_xbands = [band_index[fermi_cross_band[ii]]
                             for ii in range(self.nspin)]

        if np.sum([x.size for x in self.fermi_xbands]) == 0:
            raise ValueError(
                "No surface found at {:8.4f} eV!".format(self.efermi))


    def read_bxsf(self):
        '''
        Read band energies from VASP EIGENVAL file.
        '''
        from skeaf_a2_bxsf_parser import BxsfParser
        dat = BxsfParser(infile=self._fname)

        self.nspin = 1 			# pretend it is spin degenerate
        self.nbnds = dat.numBands 	# number of bands: an integer
        self.ir_nkpts = len( dat.kpoints_fs ) 	# or len( dat.kpoints )
        self.kmesh = dat.nkfs_dim 	# note the kmesh is 12*12*12 in KPOINTS, but 13*13*13 in bxsf file
        assert self.ir_nkpts == self.kmesh[0]*self.kmesh[1]*self.kmesh[2]

        self.efermi = dat.fermi_energy 	# fermi energy read from .bxsf
        self.reclat = dat.reclat 	# reciprocal lattice vector

        self.ir_kpath = dat.kpoints_fs	# the kpoints with BZ edge: kpoint(kx*ky*kz, iband)
        self.ir_kpath2 = dat.kpoints # the kpoints without BZ edge: kpoints(fewer, iband), fewer than kx*ky*kz

        self.ir_ebands = dat.bandEnergy_fs[np.newaxis, :, :] # dat.bandEnergy_fs is the energies with BZ edge: energy(kx*ky*kz ,iband)
        self.ir_ebands2 = dat.bands[np.newaxis,:,:] # bands is the energies without BZ edge: energy(fewer, iband), same fewer
        #self.bandData = dat.bandData 	# band energy on the k grid for i_th band: energy(iband, kx,ky,kz)

        self.bxsf_to_ebands() # Don't want the repeated values on BZ edge

        #self.grid_to_ir_map = np.arange(self.ir_nkpts)
        self.emax = self.ir_ebands.max()
        self.emin = self.ir_ebands.min()

    def bxsf_to_ebands(self):
        self.kmesh2 = self.kmesh-1

        # in self.ir_kpath, z-index runs the fastest (increase z first, then y, last x)
        # in IBZKPT generated by VASP ISYM=-1, x-index runs the fastest (increase x first, then y, last z), self.fs_kpath below is the corresponding kpath

        # I wanted to do convertion (self.ir_ebands in the order of IBZKPT), because I did not find how to reorder the shape
        # I gave up this, because it is too complicated. I found where the order is incorrectly shifted
        #self.fs_kpath_x = np.linspace(0,1, self.kmesh[0] )
        #self.fs_kpath_x = self.fs_kpath_x-np.round(self.fs_kpath_x)
        #self.fs_kpath_x = self.fs_kpath_x[:-1] # remove the last, which is 1
        #self.fs_kpath_x = np.repeat([self.fs_kpath_x], self.kmesh2[1]*self.kmesh2[2], axis=0).flatten()

        #self.fs_kpath_y = np.linspace(0,1, self.kmesh[1] )
        #self.fs_kpath_y = self.fs_kpath_y-np.round(self.fs_kpath_y)
        #self.fs_kpath_y = self.fs_kpath_y[:-1] # remove the last, which is 1
        #self.fs_kpath_y = np.repeat([np.repeat(self.fs_kpath_y, self.kmesh2[0])],self.kmesh2[2],axis=0).flatten()

        #self.fs_kpath_z = np.linspace(0,1, self.kmesh[2] )
        #self.fs_kpath_z = self.fs_kpath_z-np.round(self.fs_kpath_z)
        #self.fs_kpath_z = self.fs_kpath_z[:-1] # remove the last, which is 1
        #self.fs_kpath_z = np.repeat([self.fs_kpath_z], self.kmesh2[0]*self.kmesh2[1]).flatten()

        #self.fs_kpath = np.stack([self.fs_kpath_x, self.fs_kpath_y, self.fs_kpath_z]).T

        self.kmesh = self.kmesh2
        self.ir_kpath = self.ir_kpath2
        self.ir_ebands = self.ir_ebands2


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
