#!/bin/bash

echo checking command line arguments '(use "Runme.sh BRATS-LIKE-DIR")...'

test -d "$1" || exit 1

echo Working on dataset "$1"

./examples/High-grade-Glioblastoma_BRATS/run_analysis.sh "$1" PrepareSliceExtraction.imgql
(cd src && make && ./bin/release/netcoreapp3.1/linux-x64/VoxLogicA ../BRATS)
find -L "$1" -name "*alike*" -print0 | 
while read -d $'\0' FILE
do OUTFILE=$(dirname $FILE)/$(basename $FILE .png)-scaled.png
    convert $FILE -scale 800% $OUTFILE
done
#./examples/High-grade-Glioblastoma_BRATS/run_analysis.sh BRATS GBM-HGG-TACAS19-modified-for-uint16.imgql
