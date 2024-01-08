#!/usr/bin/env sh

### Can plot multiple minimization info
#sscha-plot-data.py minim_info

if [ -z $1 ]; then
    files=( `ls minim_* | grep -oP '(?<=).*(?=\.dat)'` )
    sscha-plot-data.py ${files[@]}
    echo used ${files[@]}
else
    sscha-plot-data.py $@
fi

echo -e '\t' generated sscha_freq.png and sscha_minim.png
echo remotemulti sscha_freq.png sscha_minim.png

echo 'if not plot, need to edit sscha-plot-data.py file'
## after both lines with "fig_data.tight_layout()"
## add
## plt.savefig('sscha_minim.png')
## plt.savefig('sscha_freq.png')
