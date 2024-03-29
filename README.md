- 👋 Hi, I’m @chenyubi14
- 👀 My expertise are VASP, shell, python. I mainly work on first-principles calculations, espectially phonon, thermal transport, defect calculations, etc.
- 🌱 Vasptools contains general functions like density of state calculations, wavefunction output, and also specific functions that is useful only if you have the same need as me.
- 📫 How to reach me: post an issue on github. I don't want to get emails.

<!---
chenyubi14/chenyubi14 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->

I post every function I developed, but they are sometimes one-time used. I am tired of deleting them, so you may ignore the extra functions. I am also too lazy to write README for every function I wrote. I will do it at request by Wechat/email/Slack.

I recommend download the scripts in a single folder, so add `export SCRIPT=/path/to/scripts` in your ~/.bashrc or ~/.bash_profile files. `pymatgen` needs to be installed, along with many other python packages.

# VASP inputs
I have a special INCAR format. You may look at INCAR_template for an example. The fixed INCAR format has several benefits: 

(1) You can directly `diff path1/INCAR path2/INCAR` in terminal to see the difference of two jobs.

(2) You will have a way to edit INCAR easily, as provided by my code.

If you want to make your own template, it is very easy. Just edit INCAR_template, and run `python incar_generate.py`, so the file class0_incar.py storing INCAR comments will be updated with new formats following INCAR_template. Remember to not use `=` in INCAR_template randomly, because incar_generate.py uses `=` to identify a tag.

# frequently used functions and commands

`python $SCRIPT/op_bs_dos.py` for submit a job to calculate density of states

`python $SCRIPT/draw_dos1_element.py` can plot the density of state plot

`python $SCRIPT/update_edit_incar.py` can update the INCAR file to the newest format

`python $SCRIPT/update_edit_incar.py mode`: please read the file update_edit_incar.py. It contains many modes to fit in different situations. 
For example, if you want to change one tag for many directories. Create a mode in the update_edit_incar.py file, and run this command.

`python $SCRIPT/out_wavefunc1.py + arguments`: will output a file storing the wave function of a band. The output file will be readable by VESTA

# usage for specific functions

## phonon calculations 
You have to download phonopy (https://phonopy.github.io/phonopy/), third_order (https://bitbucket.org/sousaw/thirdorder/src/master/), ShengBTE (https://www.shengbte.org/)

`sh $SCRIPT/pho_1_super.sh` will generate supercell jobs for each finite displacement file for 2nd order phonon dispersion. It will also give you the manage.sh file to submit jobs at once.

`sh $SCRIPT/pho_2_forceSets.sh` will generate the phonopy output file FORCE_CONSTANTS for 2nd order

`sh $SCRIPT/pho_3_dow_3rd.sh` will generate finite displacement for 3rd order phonon. Use `sh $SCRIPT/pho_3_dow_3rd.sh run` to generate folders.  I will also generate 'manage.sh' and 'check.sh' for submitting jobs and checking job outputs.

`python  $SCRIPT/pho_4_out_2ndforceconstant.py` will plot force_trace as a function of atom distance, for a specific atom you selected.

`python  $SCRIPT/pho_5_1*.py` or `python  $SCRIPT/pho_5_2*.py`: generate the CONTROL file as the input for ShengBTE

`python $SCRIPT/pho_7_write_bornall.py`: write the BORN file which is used in pho_5_2_generate_control_NAC.py

## electronic structure by Boltztrap
You have to download Boltztrap (https://gitlab.com/sousaw/BoltzTraP2).

`python $SCRIPT/fermi_*`: these files are just using the functions in the pymatgen (https://pymatgen.org/pymatgen.electronic_structure.boltztrap2.html)

## defect
`python $SCRIPT/draw_KS1_insideBG.py` draws the Kohn-Sham(KS) states within band gap. It is ususally useful to plot defect KS states.

# file types

##class*.py 

files starting with class: module files to be imported/used in other files

  class0_functions1,2,3.py: small functions

  class1_read.py: a class to read VASP input and output

  class2_update_input.py: a class of editting VASP input files

  class3_smaller_folder.py: a class dealing with op_*.py files, it usually generate subdirectories for operations like density of state calculations

  class95_energyf_fromTL.py: plot formation energy from transition levels, usualy from others' calculated transition levels

  class96_formationenergy.py: plot formation energy diagram from own computed jobs

  class99_last_drawmulinfo.py: a class stored information for drawing graphs

  class0_incar.py: a function write INCAR file with detailed comments.


##op*.py 

files starting with op: often used to create a subdirectory for some functions

  op_bs_dos.py: density of states (dos) calculations. Assume the current directory has a self-consistent output WAVECAR and CHGCAR, create a subdirectory called "dos_non-self", edit the INCAR in "dos_non-self" for dos calculations.


##out*.py 

files starting with out: generate useful information from VASP output file

  out_draw.py: draw data. Rotate over many folders to read data, and then get the plot information.

  out_formH.py: output the formation enthalpy for a single folder

  out_table_band.py: generate a latex table with bandgap information


##draw*.py

files starting with draw: rigorously these files should belong to out*.py, but draw*.py are too many, so I decided to have a new starting string. draw*.py have to draw a diagram, and out*.py has some ouputs other than a diagram.


##energyf*.py

files starting with energyf: plot formation energy diagrams. Please refer to another repository (chenyubi14/easy_defect_formation_energy). It should be contained in draw_ files, but it may contain too many different materials -> a separate file format might be neater.
