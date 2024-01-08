#!/usr/bin/env python

import numpy as np
from ase.io import read
import os
import sys
import math
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
#plt.figure(figsize=(4,6))
#fig = plt.figure()
#ax = plt.axes(projection='3d')
fig = plt.figure()#figsize=(6, 6))
ax = fig.add_subplot(111, projection='3d')


print('2pi is NOT in k definition!')
if len(sys.argv)<2:
	print('Should enter bxsf file name!')
	sys.exit()


filename='results_orbitoutlines_invAng.out'
#np.seterr(all='ignore')
freq=np.genfromtxt(filename, dtype=str, skip_header=1, invalid_raise=False)
# "Slice   =     84 , Freq(kT, average of all copies)              =   0.0074"
data=np.genfromtxt(filename, dtype=str, skip_header=5, invalid_raise=False)
# 3-component data points
if len(freq.shape) < 2: # only one extremal orbit. freq will be an one-dimensional array
	freq = freq[np.newaxis,:] # make it a two-dimensional array, like the multi orbital case
# data looks like [ [kx,ky,kz],[x1,y1,z1],[x2,y2,z2],... [kx,ky,kz],[x1,y1,z1],[x2,y2,z2],..]
sep = np.where(data == ['kx', 'ky', 'kz']) # (array([69, 69, 69]), array([0, 1, 2]))
sep = np.unique(sep[0])

left=0
# right=0 # no need for this initialization
sep = np.append(sep,len(data)+1 )
max_b = np.max([0]) 
for i, sep_i in enumerate(sep): # initial and final is not well considered yet
        right = sep_i - 1
        orb_i = data[left:right] # [sep[i]+1, sep[i+1]-1]
        orb_i = np.vstack([orb_i,orb_i[0]]) # close the loop
        orb_i = orb_i.astype(float)
        orb_i = orb_i / 2 / np.pi # the BZ vectors don't have 2pi
        left = sep_i + 1
        # the current order of defining left and right will avoid the judgement of i+1 at the end
        ax.plot3D(orb_i[:,0], orb_i[:,1], orb_i[:,2],label= "%.2f T" % ( float(freq[i,-1])*1000 ) )
        max_b = np.max(np.append(np.abs(orb_i).flatten(), max_b)) # between this orb_i and max_b, find the larger one
        #ax.scatter3D(orb_i[:,0], orb_i[:,1], orb_i[:,2],label= "%s T" % ( float(freq[i,-1])*1000 ) )

#plt.legend()

from pathlib import Path
path = Path(filename)
dir_w = path.parent.absolute()
with open(str(dir_w/'config.in'), 'r') as f:
        lines=f.readlines()
        theta1,theta2,phi1,phi2, numrots=lines[10:15] # read as string
        phi1 = float(phi1) # convert 
        theta1 = float(theta1) 
        phi2 = float(phi2)
        theta2=float(theta2)
        numrots=int(numrots)

#######################################################
# plot the B field direction
inputname='config.in'
inputs=np.genfromtxt(inputname, dtype=float, skip_header=1, invalid_raise=False)
theta1=inputs[2]
phi1=inputs[3]
def polar2cart(r, polar_angle, azimuthal_angle):
        polar = polar_angle / 180 * np.pi
        azimuthal = azimuthal_angle / 180 * np.pi
        return np.array([
                r * math.sin(polar) * math.cos(azimuthal),
                r * math.sin(polar) * math.sin(azimuthal),
                r * math.cos(polar) ])
vector_dir = polar2cart(6*max_b, phi1, theta1)
ax.quiver(0,0,0,*vector_dir, color='purple') # draw the B field vector
ax.text(*vector_dir, 'B',color='purple')
# plot a transparent surface
#xx, yy = np.meshgrid(range(10), range(10))
#z = (-normal[0] * xx - normal[1] * yy - d) * 1. /normal[2]
#######################################################


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


    def show_fermi_surf(self,max_b, cell='bz', fbcell=None, plot='mpl',zoom_in=False,
                      cmap='Spectral'):
        '''
        Plotting the Fermi surface within the BZ using matplotlib.
        supported color schemes
'Accent', 'Accent_r', 'Blues', 'Blues_r', 'BrBG', 'BrBG_r', 'BuGn', 'BuGn_r', 'BuPu', 'BuPu_r', 'CMRmap', 'CMRmap_r', 'Dark2', 'Dark2_r', 'GnBu', 'GnBu_r', 'Greens', 'Greens_r', 'Greys', 'Greys_r', 'OrRd', 'OrRd_r', 'Oranges', 'Oranges_r', 'PRGn', 'PRGn_r', 'Paired', 'Paired_r', 'Pastel1', 'Pastel1_r', 'Pastel2', 'Pastel2_r', 'PiYG', 'PiYG_r', 'PuBu', 'PuBuGn', 'PuBuGn_r', 'PuBu_r', 'PuOr', 'PuOr_r', 'PuRd', 'PuRd_r', 'Purples', 'Purples_r', 'RdBu', 'RdBu_r', 'RdGy', 'RdGy_r', 'RdPu', 'RdPu_r', 'RdYlBu', 'RdYlBu_r', 'RdYlGn', 'RdYlGn_r', 'Reds', 'Reds_r', 'Set1', 'Set1_r', 'Set2', 'Set2_r', 'Set3', 'Set3_r', 'Spectral', 'Spectral_r', 'Wistia', 'Wistia_r', 'YlGn', 'YlGnBu', 'YlGnBu_r', 'YlGn_r', 'YlOrBr', 'YlOrBr_r', 'YlOrRd', 'YlOrRd_r', 'afmhot', 'afmhot_r', 'autumn', 'autumn_r', 'binary', 'binary_r', 'bone', 'bone_r', 'brg', 'brg_r', 'bwr', 'bwr_r', 'cividis', 'cividis_r', 'cool', 'cool_r', 'coolwarm', 'coolwarm_r', 'copper', 'copper_r', 'cubehelix', 'cubehelix_r', 'flag', 'flag_r', 'gist_earth', 'gist_earth_r', 'gist_gray', 'gist_gray_r', 'gist_heat', 'gist_heat_r', 'gist_ncar', 'gist_ncar_r', 'gist_rainbow', 'gist_rainbow_r', 'gist_stern', 'gist_stern_r', 'gist_yarg', 'gist_yarg_r', 'gnuplot', 'gnuplot2', 'gnuplot2_r', 'gnuplot_r', 'gray', 'gray_r', 'hot', 'hot_r', 'hsv', 'hsv_r', 'inferno', 'inferno_r', 'jet', 'jet_r', 'magma', 'magma_r', 'nipy_spectral', 'nipy_spectral_r', 'ocean', 'ocean_r', 'pink', 'pink_r', 'plasma', 'plasma_r', 'prism', 'prism_r', 'rainbow', 'rainbow_r', 'seismic', 'seismic_r', 'spring', 'spring_r', 'summer', 'summer_r', 'tab10', 'tab10_r', 'tab20', 'tab20_r', 'tab20b', 'tab20b_r', 'tab20c', 'tab20c_r', 'terrain', 'terrain_r', 'turbo', 'turbo_r', 'twilight', 'twilight_r', 'twilight_shifted', 'twilight_shifted_r', 'viridis', 'viridis_r', 'winter', 'winter_r'
        '''
        savefig='results_surface_orbit_%.3f.pdf' % self.efermi
        cmap = 'Purples' 

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


        if plot.lower() == 'mpl':
            ############################################################
            # Plot the Fermi surface using matplotlib
            ############################################################
            import matplotlib as mpl
            import matplotlib.pyplot as plt
            from mpl_toolkits.mplot3d import Axes3D
            from mpl_toolkits.mplot3d.art3d import Poly3DCollection

            ############################################################

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
                    if zoom_in == False:
                        maximum_b = np.max([b1,b2,b3]) * 0.6 
                    else:
                        maximum_b = max_b
                    bi = bcell[ii]
                    bi = bi/np.linalg.norm(bi) * maximum_b
                    ax.plot([0, bi[0] ], [0, bi[1] ], [0, bi[2]],
                            color=basis_vector_clrs[ii], lw=1.5)
                    ax.text(bi[0], bi[1], bi[2],
                            basis_vector_labs[ii])
            else:
                fbcell = np.loadtxt(fbcell)
                assert fbcell.shape == (3,3), 'the rec_latt stored in fbcell has a wrong format'
                for ii in range(3):
                    if zoom_in == False:
                        maximum_b = np.max([b1,b2,b3]) * 0.6 
                    else:
                        maximum_b = max_b
                    bi = fbcell[ii]
                    bi = bi/np.linalg.norm(bi) * maximum_b
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
                                                                       level=self.efermi, #step_size=0.1,
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
                                           alpha=0.03, color=mpl.cm.get_cmap(cmap)(nn(cc)))
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
                if zoom_in == False:
                    max_b = np.max([b1,b2,b3]) * 0.5 
                ax.set_xlim(-max_b,max_b)
                ax.set_ylim(-max_b,max_b)
                ax.set_zlim(-max_b,max_b)
            else:
                ax.set_xlim(0, b1)
                ax.set_ylim(0, b2)
                ax.set_zlim(0, b3)

            ax.set_title('Fermi Energy: %.4f Rydberg\nB // polar:%.2f azimuthal:%.2f' % (self.efermi, phi1, theta1)  )
            #             fontsize='small')

            # plt.tight_layout()
            ax.set_axis_off()
            if zoom_in == True:
                savefig = savefig[:-3] + 'zoom_in.pdf'
            print('figure saved as %s' % (savefig) )
            plt.savefig(savefig, dpi=600,transparent=True)
            plt.show()
            ############################################################
        elif plot.lower() == 'mayavi':
            from mayavi import mlab
            # from tvtk.tools import visual

            fig = mlab.figure(size=(800, 800))
            # visual.set_viewer(fig)

            # for b in bcell:
            #     x, y, z = b
            #     ar1 = visual.Arrow(x=y, y=y, z=z)
            #     arrow_length = np.linalg.norm(b)
            #     ar1.actor.scale=[arrow_length, arrow_length, arrow_length]
            #     ar1.pos = ar1.pos/arrow_length
            #     ar1.axis = [x, y, z]

            ############################################################
            # Plot the Brillouin Zone
            ############################################################

            bz_line_width = b1 / 200
            # The BZ outlines
            for xx in l:
                mlab.plot3d(xx[:, 0], xx[:, 1], xx[:, 2],
                            tube_radius=bz_line_width,
                            color=(0, 0, 0))

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
                        faces_in_fs = faces[np.all(verts_in_bz[faces], axis=1)]

                        # keeps the vertices on the Fermi surface and remove all
                        # the other vertices
                        vertices_old_id = np.unique(faces_in_fs)
                        vertices_new_id = range(vertices_old_id.size)
                        old_new_map = dict(np.c_[vertices_old_id, vertices_new_id])

                        verts_cart = verts_cart[vertices_old_id]
                        faces_in_fs = [[old_new_map[v] for v in f] for f in faces_in_fs]
                    else:
                        nx, ny, nz = b3d.shape
                        # make band energies periodic in primitive cell
                        # b3d = np.tile(b3d, (2,2,2))[:nx+1, :ny+1, :nz+1]
                        b3d = np.pad(b3d, (0,1), mode='wrap') # mayby a little faster?

                        # https://scikit-image.org/docs/stable/api/skimage.measure.html#skimage.measure.marching_cubes_lewiner
                        verts, faces_in_fs, normals, values = marching_cubes(b3d,
                                                                       level=self.efermi,
                                                                       spacing=(
                                                                           b1/nx, b2/ny, b3/nz)
                                                                       )
                        verts_cart = np.dot(
                            verts / np.array([b1, b2, b3]),
                            bcell
                        )
                    # cc = np.linalg.norm(np.sum(verts_cart[faces_in_fs], axis=1), axis=1)
                    # kk = np.linalg.norm(verts_cart, axis=1)
                    # print(cc.min(), cc.max())
                    # print(kk.min(), kk.max())
                    mlab.triangular_mesh(verts_cart[:,0], verts_cart[:,1], verts_cart[:,2],
                                         faces_in_fs,
                                         colormap='rainbow',
                                         opacity=1.0,
                                         scalars=np.linalg.norm(verts_cart, axis=1),
                                         # vmin=cc.min(), vmax=cc.max()
                                         )

            mlab.orientation_axes()
            mlab.savefig(savefig)
            mlab.show()
        else:
            raise ValueError("Plotting method should be 'mpl' or 'mayavi'!")
        return maximum_b






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

        self.bxsf_to_ebands()

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
    arg.add_argument('--plot', dest='plot', action='store', type=str,
                     default='mpl', choices=[ 'mpl', 'mayavi'],
                     help='Fermi surface plotting method')
    arg.add_argument('--zoom', dest='zoom', action='store', type=int,
                     default=0, choices=[ 0, 1],
                     help='1: draw a zoomed-in plot, 0: no zoom in')

    return arg.parse_args(cml)


def main(max_b, cml): # cml: command line labels
    p = parse_cml_args(cml)

    fs = ebands3d(bxsf=p.bxsf, efermi=p.efermi)
    maximum_b=fs.show_fermi_surf(max_b, cell=p.cell, fbcell=p.fbcell, zoom_in=p.zoom)


if __name__ == "__main__":
    maximum_b=main(max_b, sys.argv[1:])

print('B field vector @ polar=%s azimuthal=%s' % (phi1, theta1) )
