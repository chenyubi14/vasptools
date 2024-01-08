#!/usr/bin/env python
import matplotlib.pyplot as plt
import numpy as np
import h5py as h5
from matplotlib.colors import LogNorm

# open the sqe file
f = h5.File('outfile.sqe.hdf5','r')
# get axes and intensity
x = np.array(f.get('q_values'))
y = np.array(f.get('energy_values'))
gz = np.array(f.get('intensity'))
# add a little bit so that the logscale does not go nuts
gz=gz+1E-2
# for plotting, turn the axes into 2d arrays
gx, gy = np.meshgrid(x,y)
# x-ticks
xt = np.array(f.get('q_ticks'))
# labels for the x-ticks
xl = f.attrs.get('q_tick_labels').split()
# label for y-axis
yl = "Energy ("+f.attrs.get('energy_unit')+")"

plt.pcolormesh(gx, gy, gz, norm=LogNorm(vmin=gz.min(), vmax=gz.max()), cmap='viridis')
# set the limits of the plot to the limits of the data
plt.axis([x.min(), x.max(), y.min(), y.max()])
plt.xticks(xt,xl)
plt.ylabel(yl)

plt.show()
