
# In[1]:
import sys
import os

sys.path.append(os.environ['SCRIPT'])
from class3_smaller_folders import smaller_one_folder


# In[2]:

# Goal: read source files in sources/, modity its accuricies
# not very useful function
# the folders created are energyf_(element)*
# If enter element "O", will only regenerate folders starting with "energyf_O"*

element=''
if len(sys.argv)>1:
	element=sys.argv[1]

# formation enthalpy calculations
submit = 0 # use local and submit by yourself during terminal
pwd = os.environ['PWD'] + '/' # current working directory

calc_ops=smaller_one_folder(pwd) # calculation operations
calc_ops.calc_miu4energyf(submit,element=element)
