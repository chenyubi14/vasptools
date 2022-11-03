- 👋 Hi, I’m @chenyubi14
- 👀 My expertise are VASP, shell, python. I mainly work on first-principles calculations, espectially phonon, thermal transport, defect calculations, etc.
- 🌱 Vasptools contains general functions like density of state calculations, wavefunction output, and also specific functions that is useful only if you have the same need as me.
- 📫 How to reach me: post an issue on github. I don't want to get emails.

<!---
chenyubi14/chenyubi14 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->

I post every function I developted, but they are sometimes one-time used. I am tired of deleting them, so you may ignore the extra functions.

I recommend download the scripts in a single folder, so add `export SCRIPT=/path/to/scripts` in your ~/.bashrc or ~/.bash_profile files.

# VASP inputs
I have a special INCAR format. You may look at INCAR_template for an example. The fixed INCAR format has several benefits: 

(1) You can directly `diff path1/INCAR path2/INCAR` in terminal to see the difference of two jobs.

(2) You will have a way to edit INCAR easily, as provided by my code.

If you want to make your own template, it is very easy. Just edit INCAR_template, and run `python incar_generate.py`, so the file class0_incar.py, which stores INCAR tags with comments, will be updated with new formats. Remember to not use `=` randomly, because incar_generate.py uses `=` to identify a tag.

# frequently used functions and commands

`python $SCRIPT/op_bs_dos.py` for submit a job to calculate density of states
`python $SCRIPT/draw_dos1_element.py` can plot the density of state plot


`python $SCRIPT/draw_KS1_insideBG.py` draws the Kohn-Sham(KS) states within band gap. It is ususally useful to plot defect KS states.


## file types
class*.py (files starting with class): module files to be imported/used in other files

  class0_functions1,2,3.py: small functions

  class1_read.py: a class to read VASP input and output

  class2_update_input.py: a class of editting VASP input files

  class3_smaller_folder.py: a class dealing with op_*.py files, it usually generate subdirectories for operations like density of state calculations

  class95_energyf_fromTL.py: plot formation energy from transition levels, usualy from others' calculated transition levels

  class96_formationenergy.py: plot formation energy diagram from own computed jobs

  class99_last_drawmulinfo.py: a class stored information for drawing graphs

  class0_incar.py: a function write INCAR file with detailed comments.



# generate file descriptions. Generate supercells like POSCAR, INCAR, KPOINTS for various defects
# usage: calling at vasp-job directories. e.g. python $SCRIPT/generate_supercell.py
generate_supercell.py  # establish supercells

# intermediate(inter) file descriptions. Edit or modify vasp files based on specific requirements, not submitting jobs.
# usage: calling at vasp-job directories. e.g. python $SCRIPT/inter_supercell.py
inter_defect_update.sh #  change DEFECT with some specific functions


# update file descriptions. update or edit input files 
# usage: calling at vasp-job directories e.g. python $SCRIPT/update_edit_incar.py
update_edit_incar.py
update_edit_incar.py


# operation(op) file descriptions. Submit jobs to calculate something
# usage: calling at vasp-job directories. e.g. python $SCRIPT/op_draw.py
op_var_test.py: vary a variable in an input file and run simulations
op_bs_dos.py: do non-self-consistent simulations to compute density of states (DOS) and band structure (BS)
op_draw.py: draw graphs for a variable, DOS
op_table_band.py: output a latex table with band information
op_formH.py: output formation enthalpy


# output file descriptions. Output file data
out_draw.py: draw data. Rotate over many folders to read data, and then get the plot information.
out_formH.py: output the formation enthalpy for a single folder
out_table_band.py: generate a latex table with bandgap information


# draw plots. Save data in 'savedDATA' folder
extract-bandStructure.py: 2nd step of BS drawing
draw_var.py


# energyf drawing. It should be contained in draw_ files, but it may contain too many different materials -> a separate file format might be neater.
energyf_wbeo_native.py
