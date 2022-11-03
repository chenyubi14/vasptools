import numpy as np
import os
import shutil
from pathlib import Path


def find_files(direc, header='', var='', middle='', remove=True, avoid=''): # find directories related with convergence tests
    # return a list of directories started with header='converge_'
    '''
    header = '' means find folders without header
    var = '' means find folder names without 'var' at their ends
    avoid = '' the pattern to avoid
    '''
    if direc[-1] != '/':
        direc=direc+'/'
    file_names=os.listdir(direc)
    existed_folders = []
    for i in range(len(file_names)):
        if file_names[i][:len(header)] == header and (file_names[i][-len(var):] == var or var == '') and (middle in file_names[i]):  # find folders with certain pattern
            if os.path.isdir(direc+file_names[i]) and file_names[i][-1] != '/':  # add / to directory names
                file_names[i] = file_names[i]+'/'
            if (avoid == '') or (avoid not in file_names[i]): # if the pattern to avoid does not exist
                existed_folders.append(file_names[i]) # include the file_name if there is nothing to avoid in this pattern
            if remove:  # remove folders with the same name
                shutil.rmtree(file_names[i]) # os.remove('filename'), os.rmdir('foldername')
    existed_folders = sorted(existed_folders)
    return existed_folders


def read_incar(folder, allkeyword=0, incar='INCAR'):
    ''' 
    read all variables of INCAR from folder to output a dictionary
    '''
    #if folder[-1] != '/':
    #    folder=folder+'/' # make sure the folder has correct form: .../INCAR
    #fil=folder+incar
    folder = Path(folder)
    fil = folder / incar
    with open(fil, 'r') as f:
        lines = f.readlines() # lines is a LIST, different line contents are separated with '\n' at the end
    dictionary = {} # include keywords in comments
    dictionary2 = {} # exclude keywords in comments
    for i in range(len(lines)):
        splitted=lines[i].split('=') # if contains keyword
        if len(splitted)>1:
            keyword=splitted[0].split('#')[-1].split()[-1] # keyword in INCAR: the changes are '#MAGMOM'->'MAGMOM' 
            default_val=splitted[1].split('#')[0].split('!')[0] # values of keywords
            default_val=' '.join(default_val.split()) # get rid of '\n' and undesired spaces
            dictionary[keyword]=default_val # add to dictionary
            if ('#' not in splitted[0]) and ('!' not in splitted[0]): # this keyword is not in comments
                dictionary2[keyword] = default_val
    if allkeyword:
        return dictionary
    else:
        return dictionary2


def pythonsubmit(folder,submit=0):
    if submit !=0:
        os.chdir(folder) # cwd: current working folder
        scriptfile=submit-1
        testsubmit=['SUBMIT'] # testsubmit[scriptfile=0]='SUBMIT'
        submitjob=os.environ[testsubmit[scriptfile]]
        print('use %s' % (testsubmit[scriptfile]))
        os.system('sbatch %s > job.number' % (submitjob) )
        #print('1')
        os.system('cp %s submit.job' % (submitjob) )
        os.system('cat job.number')
        os.system('echo %s >> %s/jobs.number' % (os.getcwd(), os.environ['HOME']))
        os.system("cat job.number | sed -n '/Submitted/p' >> %s/jobs.number" % (os.environ['HOME']))
        os.system("echo SUBMITTIME=$(date --iso-8601=ns) >> %s/jobs.number" % (os.environ['HOME']))
        #print('2')

def savedata(folder2nd,x,y,xname='x',yname='y',header=''):
    #mydic={'x':x,'y':y,'xname':xname,'yname':yname}
    if folder2nd[-1] != '/':
        folder2nd=folder2nd+'/' 
    x=np.round(x,6)
    y=np.round(y,6)
    #hierdirecs = parent_folder(folder2nd, hier=10)
    #assert len(hierdirecs) >= 2, 'You are not in the correct folder. Cannot save data to the folder savedDATA'
    datafilname='%s%s_%s_%s.py' % (folder2nd+'savedDATA/',header,xname,yname)
    #datafilname='%s%s-%s_%s.py' % (folder2nd,header,xname,yname)
    print('save data fil:%s' % datafilname)
    with open(datafilname, 'w') as f:
        f.write('import numpy as np\n\n')
        f.write('x=np.array(%s) \nxname=\'%s\'\n' % (x.tolist(),xname))
        f.write('y=np.array(%s) \nyname=\'%s\'\n' % (y.tolist(),yname))
        f.write(r"#print('%s=%s\n%s=%s\n'%(xname,x,yname,y))"+'\n')
