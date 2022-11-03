import numpy as np
import os
path=os.environ['SCRIPT']

'''
generate incar.py which edits INCAR files with comments included

read INCAR_template file
'''

with open('INCAR_template', 'r') as f:
    lines=f.readlines() # INCAR_template in a list of lines

content = ''.join(lines) # INCAR_template in a single string
keyword=[] # keyword in INCAR
default_vals=[] # default values of keywords
indices = [] # the index of each keyword in content
indices2 = []
accu_ind = 0 # used to find the index of keyword
for i in range(len(lines)):
    splitted=lines[i].split('=') # if contains keyword
    if len(splitted)>1:
        keyword.append(splitted[0].split('#')[-1].split()[-1]) # the changes are '#MAGMOM'->'MAGMOM' 
        default_vals.append('\''+keyword[-1]+'\':'+'\''+splitted[1].split('#')[0]+'\'') # keyword: default
        indices.append(accu_ind + lines[i].find(keyword[-1])) # index of keyword
        indices2.append(accu_ind)
    accu_ind = accu_ind + len(lines[i])
indices.append(accu_ind)
indices2.append(accu_ind)
default_vals=', \n\t\t\t'.join(default_vals)
#print(indices, '\n', indices2)
#print(keyword)

comments=[] # comments of keywords
for i in range(len(keyword)):
    com=content[indices[i]+len(keyword[i]): indices2[i+1] ]
    com=com[com.find('#'):] # delete the default value of the last keyword
    com=com.replace('\n', r'\n' )
    com=com.replace('\'s', r'\'s')
    print(keyword[i],com)
    comments.append('\'%s\':\'%s\'' % (keyword[i],com) )
comments = ', \n\t\t\t'.join(comments)
#print(comments)

with open(path+'/class0_incar.py','w') as f:
    f.write('import numpy as np \n\n\'\'\' \n Given: directory, a dictionary of input values \n Output: INCAR in the given directory \n\'\'\' \n\n')
    f.write('def write_INCAR(direc, dictionary={}, incarname=\'INCAR\'): \n\tif direc[-1]!=\'/\':\n\t\tdirec+=\'/\'\n\tcomments = {%s}\n\tdefault_vals = {%s}'% (comments, default_vals ))
    f.write('\n\twith open(direc+incarname, \'w\') as f:\n\t\tfor key in default_vals.keys():\n\t\t\tif key in dictionary:\n\t\t\t\tf.write(\'%s%s=%s %s\' % ( \'\', key, str(dictionary[key]), comments[key]) )\n\t\t\telse:\n\t\t\t\tf.write(\'%s%s=%s%s\' % ( \'# \', key, default_vals[key], comments[key]) ) \n#write_INCAR(\'./\') ' )
