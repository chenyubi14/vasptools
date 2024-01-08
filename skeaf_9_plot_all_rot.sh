#!/bin/sh

cwd=$PWD
for f in rota_*; do
	if [ -d $f ] ; then
		num=${f[@]:5}
		f2=rotb_$num
		echo $f $num
		## combine rota_ and rotb_
		python $SCRIPT/skeaf_4_plot_oscillation_full_180degree.py  $f/results_freqvsangle.out $f2/results_freqvsangle.out
		## only plot rota_
		#cd $cwd/$f
		#python $SCRIPT/skeaf_3_plot_oscillation_angle_singleRotation.py results_freqvsangle.out
		#mv $cwd/$f/results_freqvsangle.pdf $cwd/$f/rota_${num}.results_freqvsangle.pdf
		## only plot rotb_
		#cd $cwd/$f2
		#python $SCRIPT/skeaf_3_plot_oscillation_angle_singleRotation.py results_freqvsangle.out
		#mv $cwd/$f2/results_freqvsangle.pdf $cwd/$f2/rotb_${num}.results_freqvsangle.pdf
		
		cd $cwd
	fi
done

#for f in geo*; do
#	if [ -d $f ] ; then
#		num=${f[@]:4}
#		echo $f $num
#		cd $cwd/$f
#		python $SCRIPT/skeaf_8_orbit_visualization.py results_orbitoutlines_invAng.out
#		mv $cwd/$f/results_orbitoutlines_invAng.pdf $cwd/$f/results_orbitoutlines_invAng_geo${num}.pdf
#		
#		cd $cwd
#	fi
#done
