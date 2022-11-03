- 👋 Hi, I’m @chenyubi14
- 👀 My expertise are VASP, shell, python. I mainly work on first-principles calculations, espectially phonon, thermal transport, defect calculations, etc.
- 🌱 Vasptools contains general functions like density of state calculations, wavefunction output, and also specific functions that is useful only if you have the same need as me.
- 📫 How to reach me: post an issue on github. I don't want to get emails.

<!---
chenyubi14/chenyubi14 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->

I post every function I developted, but they are sometimes one-time used. I am tired of deleting them, so you may ignore the extra functions.


# module file descriptions
functions.py: small functions that is useful for many situations
class1_read.py: a class of read input and output files
class2_update_input.py: a class of editting input files
class3_smaller_folder.py: a class dealing with operations on smaller folders like 3rd and 4th folders
class4_supercellgenerate.py: generate supercell files and edit its input
class98_intermediate_data.py: get intermediate data from unit cells and use them in supercell
class99_last_drawinfo.py: a class stored information for drawing graphs
incar.py: write INCAR with comments and fixed format. All the comments are stored in this file. This file can be generated from incar_generate.py  INCAR_template in this folder old+incar. Simply use python incar_generate.py

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
