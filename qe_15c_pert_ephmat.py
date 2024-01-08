import perturbopy.postproc as ppy
import matplotlib.pyplot as plt

gaas_ephmat = ppy.Ephmat.from_yaml('gaas_ephmat.yml')

plt.rcParams.update(ppy.plot_tools.plotparams)
gaas_ephmat.qpt.add_labels(ppy.lattice.points_fcc)

fig, ax  = plt.subplots()
gaas_ephmat.plot_ephmat(ax,log=True)
#plt.show()
plt.savefig('gaas_ephmat.png')
