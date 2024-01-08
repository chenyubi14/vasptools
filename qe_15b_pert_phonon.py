import perturbopy.postproc as ppy
import matplotlib.pyplot as plt

gaas_phdisp = ppy.Phdisp.from_yaml('gaas_phdisp.yml')

# Create a figure and axis for plotting
fig, ax  = plt.subplots()

# Optional, used to format the plot
plt.rcParams.update(ppy.plot_tools.plotparams)

# Optional, used to label the q-points with labels for the FCC crystal structure.
# For example, [0.5, 0.5, 0.5] is the 'L' point in the FCC Brillouin zone.
gaas_phdisp.qpt.add_labels(ppy.lattice.points_fcc)

gaas_phdisp.plot_phdisp(ax)
#plt.show()
plt.savefig('gaas'_phdisp.png')
