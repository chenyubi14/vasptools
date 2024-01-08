import sys
import os
sys.path.append(os.environ['SCRIPT'])
from class0_functions1 import read_incar
from class0_functions3 import write_INFO

# update DEFECT information
# run at the 2nd-class folders 

#if len(sys.argv)!=3:
#	print('No files specified')
#sys.argv[1] == 
datatxtfile=sys.argv[1] #'wBeO_O-rich_native.txt'
targetfolder = '.'
defect_dict=read_incar(targetfolder, incar='SAVEINFO')
found=0

with open(datatxtfile,'r') as f:
	lines = f.readlines() # lines is a LIST, different line contents are separated with '\n' at the end
for i in range(len(lines)):
	line = lines[i]
	splitted=line.split('=') # if contains keyword
	if len(splitted)>1:
		keyword,keywordval = splitted
		#keyword=splitted[0] #.split('#')[-1].split()[-1] # keyword in INCAR: the changes are '#MAGMOM'->'MAGMOM' 
		#default_val=splitted[1] #.split('#')[0].split('!')[0] # values of keywords
		keywordval = ' '.join(keywordval.split())
		if keyword == 'DEFECTTYPE' and keywordval == defect_dict['DEFECTTYPE']: # read the transitions under the correct defect type
			found=1
			line_1 = lines[i+1] # 1 line below
			line_2 =lines[i+2] # 2 lines below
			keyword_1,keywordval_1 = line_1.split('=')
			keyword_2,keywordval_2 = line_2.split('=')
			keywordval_1=' '.join(keywordval_1.split()) # get rid of '\n' and undesired spaces
			keywordval_2=' '.join(keywordval_2.split())
			print(keywordval_1,'\n',keywordval_2)
			break

if found ==0:
	print('Error! The defect %s transition levels are not found' % defect_dict['DEFECTTYPE'])
defect_dict['TRANSITIONCHARGE'] = keywordval_1
defect_dict['TRANSITIONFERMILEVEL'] = keywordval_2

write_INFO(dictionary=defect_dict, targetfolder)
