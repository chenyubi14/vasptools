import numpy as np
import sys
import time
import os
from pathlib import Path as path
#import unicodedata as uni
#import multiprocessing
#import signal
sys.path.append(os.environ['SCRIPT'])

def make_folder_newname(foldername):
	with open(str(path(foldername) / 'time.info'), "r") as f:
		lines = f.readlines()
		for i in range(len(lines)-1, 0, -1):
			ll=lines[i] #.split()
			checkstring='STARTTIME='
			if checkstring in ll:
				ll=ll.split(checkstring)[-1]
				ll=ll.split(',')[0]
				foldername = foldername.split('-TIME-')[0] # If the foldername containes TIME info already
				newname = foldername + '-TIME-%s'%ll
				break
	return newname

for i in range(1, len(sys.argv)):
	foldername=sys.argv[i]
	if os.path.isdir(foldername):
		newname=make_folder_newname(foldername)
		print('mv ',foldername,newname)
		os.system('mv %s %s' % (foldername, newname))